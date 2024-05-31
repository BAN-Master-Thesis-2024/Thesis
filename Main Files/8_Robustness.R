library(tidyverse)
library(parallel)
library(tictoc)
library(anytime)
library(lubridate)
library(RecordLinkage) # Jaro-Winkler
library(stringdist) # Q-gram distance


###### Robustness check officers -----------------

# Load data for officer check
match_officers_all <- read.csv("CSV-files/match_officers_cl.csv") # all officer matches we got (our database based on last name search)
officers_real <- read.csv("name_data/officers_real.csv", sep =";") # legit cartel names from impact report and OECD charts

# Formatting the dataset for officers
officers_real$Name <- paste(officers_real$First.name, officers_real$Last.name) # Empty column
officers_real$Name <- toupper(officers_real$Name) # Names to upper letters
officers_real$First.name <- toupper(officers_real$First.name) # Names to upper letters
officers_real$Name <- str_squish(officers_real$Name) # Removing all doubble spaces
match_officers_all$name <- str_squish(match_officers_all$name) # Removing all doubble spaces

officers_real <- officers_real %>% # Remove all dublicate names (keeps only first observation)
  distinct(Name, .keep_all = TRUE)
officers_real$index <- 1:nrow(officers_real) # Add index, for easier referencing between original datasets


## Officers robustness, (want one match first name and one match last name)
cl <- makeCluster(6) # Using multiple cores 
tic() # Track time 
robust_officers <- data.frame() # Initialize an empty dataframe to store matches

# Loop through each observation in match_officers (our database)
for (i in 1:nrow(match_officers_all)) {
  
  words_DF1 <- unlist(strsplit(as.character(match_officers_all$name[i]), " ")) # Split the text into words, with space as delimiter
  match_words <- character(0) # variables to store match information
  match_count <- 0 # variables to store match information
  
  # Loop through each observation in DF2
  for (j in 1:nrow(officers_real)) {
    # Split the text into words
    words_DF2 <- unlist(strsplit(as.character(officers_real$Last.name[j]), " ")) # Split the text into words, with space as delimiter (First name from Cartel)
    words_DF3 <- unlist(strsplit(as.character(officers_real$First.name[j]), " ")) # Split the text into words, with space as delimiter (Last name from Cartel)
    
    # Count the number of similar words
    num_similar1 <- length(intersect(words_DF1, words_DF2))
    num_similar2 <- length(intersect(words_DF1, words_DF3))
    
    # Check if there is at least one match in first name and last name
    if (num_similar1 >= 1 & num_similar2 >= 1) {
      # Store the matches
      match_words1 <- paste(intersect(words_DF1, words_DF2), collapse = " ")
      match_words2 <- paste(intersect(words_DF1, words_DF3), collapse = " ")
      
      # Gives value if there is a match, so that we can save it
      match_count <- match_count + 1
      
      # Adding additional information from the match in the new database
      cartel <- officers_real$Cartel[j]
      index <- officers_real$index[j]
      name2 <- officers_real$Name[j]
      print(officers_real$Last.name[j])
    }
  }
  
  # Add the match information to the new dataframe
  if (match_count > 0) {
    robust_officers <- rbind(robust_officers, cbind(match_officers_all[i,], 
                                                                    first_name = match_words2, 
                                                                    last_name = match_words1, 
                                                                    cartel = cartel,
                                                                    index = index,
                                                                    name2 = name2))
  }
}
toc() # Track time
stopCluster(cl) # Stop cluster


robust_officers <- robust_officers %>% # Remove all dublicate names (keeps only first observation)
  distinct(index, .keep_all = TRUE)
robust_officers <- distinct(robust_officers, node_id, .keep_all = TRUE)

###### Robustness check entities -----------------

# Load data for entities check
match_entities_all <- read.csv("CSV-files/companies_cl.csv")
entities_real <- read.csv("name_data/entities_real.csv", sep =";")

### Formatting the dataset for entities 
match_entities_all$clean_name <- match_entities_all$name
entities_real$clean_name <- entities_real$Company
match_entities_all$clean_name <- str_replace_all(match_entities_all$clean_name, "[^[:alnum:]]", " ")
entities_real$clean_name <- str_replace_all(entities_real$clean_name, "[^[:alnum:]]", " ")

#       Function to remove words shorter than 4 letters
clean_column <- function(text) {
  gsub("\\b\\w{1,3}\\b", "", text)
}
match_entities_all$clean_name <- sapply(match_entities_all$clean_name, clean_column) # Removing short words
entities_real$clean_name <- sapply(entities_real$clean_name, clean_column) # Removing short words
match_entities_all$clean_name <- str_squish(match_entities_all$clean_name) # Removing all doubble spaces
entities_real$clean_name <- str_squish(entities_real$clean_name) # Removing all doubble spaces

match_entities_all <- match_entities_all %>% # Removing rows containing empty values after cleaning
  filter(clean_name != "")
match_entities_all <- match_entities_all %>% # Removing rows containing empty values after cleaning
  filter(name != "")
entities_real <- entities_real %>% # Removing rows containing empty values after cleaning
  filter(clean_name != "")
entities_real <- entities_real %>% # Removing rows containing empty values after cleaning
  filter(Company != "")

entities_real <- entities_real %>% # Remove all dublicate names (keeps only first observation)
  distinct(clean_name, .keep_all = TRUE)
entities_real$index <- 1:nrow(entities_real) # Add index, for easier referencing between original dataset

## Entities robustness, (want full match on clean name)
cl <- makeCluster(6) # Using multiple cores 
tic() # Track time 
robust_entities <- data.frame() # Initialize an empty dataframe to store matches

# Loop through each observation in match_entities (our database)
for (i in 1:nrow(match_entities_all)) {
  
  words_DF1 <- match_entities_all$clean_name[i] # Store clean entity name from our database
  match_words <- character(0) # variables to store match information
  match_count <- 0 # variables to store match information
  
  # Loop through each observation in DF2
  for (j in 1:nrow(entities_real)) {
    # Split the text into words
    words_DF2 <- entities_real$clean_name[j] # # Store clean entity name from cartels
    
    # Check if there is a full match between the two names
    if (words_DF1 == words_DF2) {
      # Store the matches
      match_words <- paste(intersect(words_DF1, words_DF2), collapse = " ")
      
      # Gives value if there is a match, so that we can save it
      match_count <- match_count + 1
      
      # Adding additional information from the match in the new database
      cartel <- entities_real$Cartel[j]
      index <- entities_real$index[j]
      clean_name2 = entities_real$clean_name[j]
      name_real = entities_real$Company[j]
      print(entities_real$Last.name[j])
      
    }
  }
  
  # Add the match information to the new dataframe
  if (match_count > 0) {
    robust_entities <- rbind(robust_entities, cbind(match_entities_all[i,], 
                                                                    clean_name2 = clean_name2,
                                                                    name_real = name_real,
                                                                    match_words = match_words,
                                                                    cartel = cartel,
                                                                    index = index))
  }
}
toc() # Track time
stopCluster(cl) # Stop cluster

robust_entities <- distinct(robust_entities, node_id, .keep_all = TRUE)


###### CALCULATING RESULT OF STRING SIMILARITIES --------------------

# To conveniently calculate Jaro-Winkler similarity, we need to use the package RecordLinkage


### Jaro_Winkler for officers and entities -------

robust_officers$jarowinkler <- jarowinkler(robust_officers$name, robust_officers$name2)

robust_entities$jarowinkler <- jarowinkler(robust_entities$name, robust_entities$name_real)

## Q-gram distance ---------------------

# Function to normalize q-gram distance
normalize_qgram_distance <- function(dist, len1, len2) {
  max_possible_distance <- len1 + len2 - 2
  return(1 - (dist / max_possible_distance))
}

# Q-gram for officers
vector1 <- robust_officers$name
vector2 <- robust_officers$name2

# Initialize a vector to store the normalized q-gram similarities
qgram_similarity_scores <- numeric(length(vector1))

# Compute normalized q-gram similarities
for (i in 1:length(vector1)) {
  qgram_distance <- stringdist(vector1[i], vector2[i], method = "qgram", q = 2)
  len1 <- nchar(vector1[i])
  len2 <- nchar(vector2[i])
  qgram_similarity_scores[i] <- normalize_qgram_distance(qgram_distance, len1, len2)
}

# Create a data frame to display the results
qgram_officers <- data.frame(
  Name_in_Vector1 = vector1,
  Name_in_Vector2 = vector2,
  Qgram_Similarity_Score = qgram_similarity_scores
)
robust_officers$q_gram <- qgram_officers$Qgram_Similarity_Score

# Q-gram for entities
vector3 <- robust_entities$name
vector4 <- robust_entities$name_real

qgram_similarity_scores <- numeric(length(vector3))

# Compute normalized q-gram similarities
for (i in 1:length(vector3)) {
  qgram_distance <- stringdist(vector3[i], vector4[i], method = "qgram", q = 2)
  len1 <- nchar(vector3[i])
  len2 <- nchar(vector4[i])
  qgram_similarity_scores[i] <- normalize_qgram_distance(qgram_distance, len1, len2)
}

# Create a data frame to display the results
qgram_entities <- data.frame(
  Name_in_Vector1 = vector3,
  Name_in_Vector2 = vector4,
  Qgram_Similarity_Score = qgram_similarity_scores
)
robust_entities$q_gram <- qgram_entities$Qgram_Similarity_Score



# Calculating the average scores

mean(robust_officers$q_gram)
median(sort(robust_officers$q_gram))
sd(robust_officers$q_gram)
min(robust_officers$q_gram)
max(robust_officers$q_gram)

mean(robust_officers$jarowinkler)
median(sort(robust_officers$jarowinkler))
sd(robust_officers$jarowinkler)
min(robust_officers$jarowinkler)
max(robust_officers$jarowinkler)

mean(robust_entities$q_gram)
median(sort(robust_entities$q_gram))
sd(robust_entities$q_gram)
min(robust_entities$q_gram)
max(robust_entities$q_gram)

mean(robust_entities$jarowinkler)
median(sort(robust_entities$jarowinkler))
sd(robust_entities$jarowinkler)
min(robust_entities$jarowinkler)
max(robust_entities$jarowinkler)

### EXTRA - (not used) Connecting entities linked to companies from companies_cl ----------------
relationships <- read_csv("ICIJ/data/relationships.csv") 
nodes_officers <- read_csv("ICIJ/data/nodes-officers.csv")

filtered_node_id_start <- relationships[relationships$node_id_start %in% robust_officers$node_id, ]
filtered_node_id_end <- relationships[relationships$node_id_end %in% robust_officers$node_id, ]
filtered_node_id <- rbind(filtered_node_id_start, filtered_node_id_end)

companies_node_id_start <- match_entities_all[match_entities_all$node_id %in% filtered_node_id$node_id_start, ]
companies_node_id_end <- match_entities_all[match_entities_all$node_id %in% filtered_node_id$node_id_end, ]
companies_node_id <- rbind(companies_node_id_start, companies_node_id_end)

robust_entities_2 <- bind_rows(robust_entities, companies_node_id)

write_csv(robust_entities_2, "robust_entities_2.csv")

robust_entities <- distinct(robust_entities, node_id, .keep_all = TRUE)
robust_entities_2 <- distinct(robust_entities_2, node_id, .keep_all = TRUE)

