## NHH - BAN-master thesis ##
# Part 2 - Finding matches in ICIJ-database

# Loading packages 
library(tidyverse)
library(parallel)
library(tictoc)
library(tm)

## ---------------------------------- ## 
### 2) Looking up last-names in ICIJ ### 
## ---------------------------------- ## 

# Loading saved data from part 1 
last_names <- read.csv("CSV-files/last_names.csv", sep = ",") %>% 
  select(Name)

# Loading leaks-data from ICIJ
nodes_officers <- read.csv("ICIJ/Data/nodes-officers.csv")
nodes_entities <- read.csv("ICIJ/Data/nodes-entities.csv")

# Converting to upper
nodes_officers$name <- toupper(nodes_officers$name)
nodes_entities$name <- toupper(nodes_entities$name)

test <- read.csv("ICIJ/Data/relationships.csv")
test2 <- read.csv("ICIJ/Data/nodes-addresses.csv")

## ------------- ## 
# Matching names 
## ------------- ## 
names <- last_names$Name # Last-names as a string 

# Officers
## ------------- ## 
match_officers <- NULL # Storing matching names in this 

cl <- makeCluster(6) # Using multiple cores 

tic() # Track time 

# Loop for matching officers
for (i in 1:length(names)) {
  
  cat("Name", names[i], "\n") # For debugging purposes
  
  # Extracting match 
  match <- nodes_officers[grepl(paste0("\\b",names[i],"\\b"), nodes_officers$name),]
  
  # Add search-term if matches were found 
  if (nrow(match) > 0) {
    
    # Add term
    match$term <- names[i]
    
    # Appending to df
    match_officers <- rbind(match_officers, match)
    cat("Matches", names[i], nrow(match), "\n") # Keeping track of nr of matches 
    
  } else {
    
    cat("No match", names[i], "\n") # For debugging-purposes 
  }

}

toc() # Track time
stopCluster(cl) # Stop cluster

write.csv(match_officers, "match_officers.csv", row.names=FALSE) # Saving results 

# Entities 
## ------------- ## 

match_entities <- NULL #

cl <- makeCluster(6) # Using multiple cores 

tic() # Track time 

# Loop for matching entities
for (i in 1:length(names)) {
  
  cat("Name", names[i], "\n") # For debugging purposes
  
  # Extracting match 
  match <- nodes_entities[grepl(paste0("\\b",names[i],"\\b"), nodes_entities$name),]
  
  # Add search-term if matches were found 
  if (nrow(match) > 0) {
    
    # Add term
    match$term <- names[i]
    
    # Appending to df
    match_entities <- rbind(match_entities, match)
    
    cat("Matches", names[i], nrow(match), "\n") # Keeping track of nr of matches 
  } else {
    
    cat("No match", names[i], "\n") # For debugging-purposes 
  }
  
}

toc() # Track time
stopCluster(cl) # Stop cluster

write.csv(match_entities, "match_entities.csv", row.names=FALSE) # Saving results

# 
match_entities <- read.csv("match_entities.csv")
match_officers <- read.csv("match_officers.csv")

# NB
# 1) Should we add an if-function that states that IDs that already has been retrieved will not be added one more time? 
# For instance, a person can be called Juan CARRASCO PEREIRA. If we have both last-names as a search-term, the person will be added two times.
# Meaning both names will count as 2 observations, when there really is only 1. 

## ------------- ## 
# Some cleaning 
## ------------- ## 

match_O <- match_officers[, c("node_id", "term")] # Selecting node-id and term
match_E <- match_entities[, c("node_id", "term")] # Selecting node-id and term 

match_combined <- rbind(match_O, match_E) # Joining matches

match_combined <- unique(match_combined) %>% 
  arrange(node_id)


# Filtering too common search-terms
docs <- VCorpus(VectorSource(match_combined$term),
                readerControl = list(reader=readPlain,
                                     language="en"))

# Converting to lower-case
docs <- tm_map(docs, content_transformer(tolower))

# Building document-term-matrix (DTM)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

# List of commons 
list_commons <- toupper(d$word[d$freq > 100])

match_officers_clean <- match_officers[!(match_officers$term %in% list_commons),] # Filtering for commons
match_entities_clean <- match_entities[!(match_entities$term %in% list_commons),] # Filtering for commons

# More filtering 
commons <- toupper(c("persons", "mexican", "arizona", "automotriz", "ambiental", "freezer", "minera", "nacionales", "alianza", ""))

match_officers_clean <- match_officers_clean[!(match_officers_clean$term %in% commons),]
match_entities_clean <- match_entities_clean[!(match_entities_clean$term %in% commons),]

# Saving
write.csv(match_officers_clean, "match_officers_cl.csv", row.names=FALSE)
write.csv(match_entities_clean, "match_entities_cl.csv", row.names=FALSE)

###### 
#### Test 1)  ####
# Loop for removing duplicated IDs
for (i in 1:nrow(match_combined)-1) { 
  
  # Extracting observation i 
  first_obs <- match_combined$node_id[i]
  
  # Extracting observation i+1
  next_obs <- match_combined$node_id[i + 1]
  
  # Remove observation if IDs are similar
  if (first_obs == next_obs) {
    
    row_remove <- match_combined[i+1]
    
  }
    
}


