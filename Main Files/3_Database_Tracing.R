## NHH - BAN-master thesis ##
# Part 3 - Tracing database 

# Loading packages
library(tidyverse)
library(countrycode)
library(stringr)
library(stargazer)
library(readr)
library(haven)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(parallel)
library(tictoc)
library(anytime)

library(humaniformat)
library(genero)
library(entity)

library(devtools)
library(locationiq)
library(postmastr)
library(ggmap) 

library(gt)
library(gtExtras)

library(lubridate)

library(skimr)

library(grid)
library(gridExtra)

## ---------------------------------- ## 
### 3) Tracing matches in database   ### 
## ---------------------------------- ## 

# Loading data 
# Matches found in part 2 
match_entities <- read.csv("CSV-files/Matches/match_entities_cl.csv")
match_officers <- read.csv("CSV-files/Matches/match_officers_cl.csv")

# ICIJ-data
nodes_addresses <- read_csv("ICIJ/Data/nodes-addresses.csv") # Addresses of nodes 
relationships <- read_csv("ICIJ/Data/relationships.csv")     # Relationships between nodes 
nodes_entities <- read_csv("ICIJ/Data/nodes-entities.csv")   # Entity-nodes 
nodes_officers <- read_csv("ICIJ/Data/nodes-officers.csv")   # Officers-nodes

## ------------------------------------------- ## 
# Extracting first-names and determining gender 
## ------------------------------------------- ##

# Patterns that indicates multiple names
patterns <- c(";", " AND ", "JOINT TENANCY", " Y/O ", " Ó ")

# Extracting common spanish names (can also be used for Colombia)
spanish_names <- names_gender_es$name

# Loop that extracts first names and determines gender 
for (i in 1:nrow(match_officers)) {
  
  # Extracting full name 
  full_name <- match_officers$name[i]
  
  # Replacing specified patterns
  full_name <- str_replace_all(full_name, paste(patterns, collapse = "|"), "##")
  
  # Test for validating whether one or multiple names are mentioned
  full_name_test <- tibble(unlist(str_split(full_name, "##"))) %>% 
    nrow() 
  
  # Assign names only to observations containing 1 name
  if (full_name_test == 1) {
    
    # Determining what is the first name for each iteration i
    match_officers$first_name[i] <- first_name(full_name)
    
    # Assigning gender 
    match_officers$gender[i] <- genero(match_officers$first_name[i])
    
    # Try to assign gender for NA-observations by reversing name 
    if (is.na(match_officers$gender[i])) {
      
      # Extracting reversed name 
      reversed_name <- format_reverse(full_name)
      
      # Extracting reversed first-name
      reverse_first_name <- first_name(reversed_name)
      
      # Assigning new values to df
      match_officers$first_name[i] <- reverse_first_name
      match_officers$gender[i] <- genero(match_officers$first_name[i])

      
    } 
    
    # Replacing "-" with "," to assign further genders 
     if (is.na(match_officers$gender[i])) {
       
       # Replacing "-" with ","
       reversed_new <- str_replace_all(reversed_name, "-", ",")
       
       reversed_new <- format_reverse(reversed_new) # Try to reverse one more time 
       
       first_name_new <- first_name(reversed_new) # Finding first name again
       
       # Assigning gender one more time 
       match_officers$first_name[i] <- first_name_new
       match_officers$gender[i] <- genero(match_officers$first_name[i])
       
       
       # Tokenizing names and searching in spanish name-database
     } 
    if (is.na(match_officers$gender[i])) {
      
      name_tokens <- tolower(unlist(str_split(match_officers$name[i], " "))) # Split name by space
      
      # Finding spanish names matches 
      matched_name <- name_tokens[name_tokens %in% spanish_names]
      
      # Assign gender if all tokens is of same gender
      if (all(matched_name == matched_name[1])) { 
        # Assigning first name and gender
        match_officers$first_name[i] <- toupper(matched_name[1]) # Using 1 match as first-name
        match_officers$gender[i] <- genero(match_officers$first_name[i]) 
      }
    }
    
    # Assign NA otherwise
  } else {
    
    # Assigning NAs
    match_officers$first_name[i] <- NA
    match_officers$gender[i] <- NA
  }
}

# Cleaning environment
rm(first_name_new, full_name, full_name_test, i, matched_name, name_tokens, patterns, reverse_first_name, reversed_name, reversed_new, spanish_names) 

write_csv(match_officers, "CSV-files/match_officers2.csv")

## ---------------------------------- ## 
# Table 1 and 2) Descriptive statistics  
## ---------------------------------- ## 

# Proportion of matches by gender 
gender_prop <- match_officers %>% 
  group_by(gender) %>% 
  tally() %>% 
  mutate(prop = n/sum(n))

# Formatting before table 
colnames(gender_prop) <- c("Gender", "Observations", "Proportion")
gender_prop$Gender[3] <- "none"

# Company-types
company_type <- match_entities %>% 
  group_by(company_type) %>% 
  tally() %>% 
  arrange(-n)

# Formatting before table 
colnames(company_type) <- c("Company Type", "Count")
company_type$`Company Type`[1] <- "Missing"

# -------- # 
## Output ## 
# -------- # 

# Gender Prop
gender_tb <- gender_prop %>% 
  gt() %>% 
 # tab_header(title = "TABLE 1: GENDER DISTRIBUTION") %>% 
  opt_all_caps() %>% 
  opt_align_table_header("left") %>% 
  tab_source_note(md("n = 4881")) %>% 
  tab_options(source_notes.font.size = 12,
              table.font.size = 16,
              heading.title.font.size = 20,
              table.border.top.color = "transparent",
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7",
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% 
  fmt_number(columns = 3,
             rows = everything(),
             decimals = 3) %>% 
  gt_highlight_rows(rows = 2, font_weight = "normal", alpha = 0.4) %>% 
  gt_highlight_rows(rows = 1, fill = "pink", font_weight = "normal", alpha = 0.4) %>% 
  gt_highlight_rows(rows = 3, fill = "lightgrey", font_weight = "normal", alpha = 0.4)

gtsave(gender_tb, "Figures/Gender_Prop.html") # Save output

# Company-Type
company_tb <- company_type %>% 
  gt() %>% 
  tab_header(title = "TABLE 3: COMPANY TYPE") %>% 
  opt_all_caps() %>% 
  opt_align_table_header("left") %>% 
  tab_source_note(md("**Table 3:** Company-types by type of search")) %>% 
  tab_options(source_notes.font.size = 12,
              table.font.size = 16,
              heading.title.font.size = 20,
              table.border.top.color = "transparent",
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7",
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% 
  data_color(columns = c(Count), # Fill color by value
             fn = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL
             ))

gtsave(company_tb, "Figures/Company_Tab.html") # Save

## ----------------------------------------------------------- ## 
# Wordcloud 1 and 2 - Top search terms for officers and entities
## ----------------------------------------------------------- ## 

# Extracting nr of matches per term 
top_terms <- match_officers %>% 
  group_by(term) %>% 
  tally()

# Generating wordcloud - Top matches officers
png(filename = "Figures/SearchTerms_officers.png",
         units = "cm",
         width = 40,
         height = 40,res = 300)

wordcloud(words = top_terms$term, 
          freq = top_terms$n,
          min.freq =  2, # Min 2 observations 
          random.order = FALSE, rot.per = 0.35,           
          colors=brewer.pal(8, "Spectral"))

dev.off()

# Generating wordcloud - Top matches entities

# Extracting nr of matches per term
top_terms2 <- match_entities %>% 
  group_by(term) %>% 
  tally()

png(filename = "Figures/SearchTerms_entities.png",
    units = "cm",
    width = 40,
    height = 40,res = 300)

wordcloud(words = top_terms2$term, 
          freq = top_terms2$n,
          min.freq =  2, # Min 2 observations 
          random.order = FALSE, rot.per = 0.35,           
          colors=brewer.pal(8, "Spectral"))

dev.off()

## ---------------------------------- ## 
# Wordcloud 3 - Company names 
## ---------------------------------- ##

# Extracting tokens
company_tokens <- str_split(match_entities$name, pattern = " ") %>%  # Split by space
  unlist() %>% 
  str_replace_all("[^[:alnum:]]", "") %>% 
  str_replace_all("[:digit:]", "") %>% 
  str_replace("LTD", "LIMITED") %>% 
  str_replace("INC", "INCORPORATED") %>% 
  tibble()

colnames(company_tokens) <- c("Word")  # Rename col

company_tokens <- company_tokens %>% 
  group_by(Word) %>% 
  tally() %>% 
  filter(nchar(Word) > 2) %>% # Removing words with less than 2 characters 
  filter(n > 8) # Filter for words with less than 8

# Wordcloud
png(filename = "Figures/Company_Tokens.png",
    units = "cm",
    width = 40,
    height = 40,res = 300)

wordcloud(words = company_tokens$Word, 
          freq = company_tokens$n,
          random.order = FALSE, rot.per = 0.35,           
          colors=brewer.pal(8, "Spectral"))

dev.off()

# Table illustrating results
colnames(company_tokens) <- c("Word", "Frequency") # Changing names 
 
company_tokens <- company_tokens[order(company_tokens$Frequency, decreasing = TRUE),] # Order by Frequency

# Table 4 - Top tokens 
comp_tokens_tb <- company_tokens[1:20, ] %>% 
  gt() %>% tab_header(title = "TABLE 4: TOP TOKENS") %>% 
  opt_all_caps() %>% 
  opt_align_table_header("left") %>% 
  tab_source_note(md("**Table 4:** TEST")) %>% 
  tab_options(source_notes.font.size = 12,
              table.font.size = 16,
              heading.title.font.size = 24,
              table.border.top.color = "transparent",
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7",
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping()

gtsave(comp_tokens_tb, "Figures/Company_Tokens.html") # Save output

## ---------------------------------- ## 
# Table - Node relations
## ---------------------------------- ##

## --------------------------- # 
# Extracting node-IDs
## --------------------------- # 

# Officer IDs
node_ids_off <- match_officers %>% 
  dplyr::select(node_id) %>% 
  unique()

node_ids_off <- c(node_ids_off$node_id) # Format as vector 

# Officer - Node-id-start relations 
node_relations_off <- relationships %>% 
  filter(node_id_start %in% node_ids_off) %>% 
  mutate(origin_match = "1_node_start") # Origin 1: Officers matches, node start

# Officer - Node-id-end relations
node_relations_off2 <- relationships %>% 
  filter(node_id_end %in% node_ids_off) %>% 
  mutate(origin_match = "1_node_end") # Origin 1: Officers matches, node end # This is only similar name

# Entities IDs
node_ids_ent <- match_entities %>% 
  dplyr::select(node_id) %>% 
  unique()

node_ids_ent <- c(node_ids_ent$node_id) # Format as vector

# Entity - Node-id-start relations 
node_relations_ent <- relationships %>% 
  filter(node_id_start %in% node_ids_ent) %>% 
  mutate(origin_match = "2_node_start") # Origin 2: Entities matches, node start # Typically similar names 

# Entity- Node-id-end relations
node_relations_ent2 <- relationships %>% 
  filter(node_id_end %in% node_ids_ent) %>% 
  mutate(origin_match = "2_node_end") # Origin 2: Entities matches, node end 

# Binding dfs
node_relationships <- rbind(node_relations_off, node_relations_off2,
                            node_relations_ent, node_relations_ent2) # Compiling relations to one df

# NB: Do we want to remove distinct-values? 
# A person could have been a shareholder of company A 10 times. If we remove distinct values, we only keep 1 of these as a relation. 
node_relationships <- node_relationships %>% 
  distinct(node_id_start, node_id_end, link, .keep_all = TRUE) 

write.csv(node_relationships, file = "CSV-files/relationships_cl.csv", row.names = FALSE) # Save for future use

## --------------------------- # 
# Extracting statistics
## --------------------------- # 
# Retrieving frequency of relations
relations <- node_relationships %>% 
  group_by(link) %>% 
  tally()

# Retrieving MAX value of same end point
max_endpoint <- node_relationships %>% 
  group_by(link, node_id_end) %>% 
  tally() 

# Retrieving MAX
max <- max_endpoint %>% 
  group_by(link) %>%  # Group by type of link
  arrange(desc(n)) %>%  # Arrange 
  slice(1) %>% # Slice top n for each group
  select(link, n)

# Retrieving SD 
sd <- max_endpoint %>% 
  group_by(link) %>% 
  summarise_at(vars(n), list(stand_d = sd))

# Retrieving mean
mean <- max_endpoint %>% 
  group_by(link) %>% 
  summarise_at(vars(n), list(avg = mean))

# Joining dfs
relations <- relations %>% 
  left_join(max,  by = "link") %>% 
  left_join(sd, by = "link") %>% 
  left_join(mean, by = "link")

# Cleaning
colnames(relations) <- c("Position", "Frequency", "Max", "SD", "Avg")

relations <- relations[, c(1, 2, 5, 4, 3)] # Reorder cols

relations <- arrange(relations, desc(Frequency)) # Reorder by freq

rm(max, sd, mean) # Clean environment

# ---------- # 
# Output 
# ---------- # 

# Selecting top 15 relationships 
relations_out <- relations %>% 
  slice(1:15)

relations_tb <- relations_out %>% 
  gt() %>% 
  tab_header(title = "TABLE 5: TOP RELATIONSHIPS") %>% # Title
  opt_all_caps() %>%  # All caps
  opt_align_table_header("left") %>% # Align header
  tab_source_note(md("**Table 5:** Most common relationships connected to matched entities & officers")) %>%  # Footnote
  tab_options(source_notes.font.size = 12, # Sice footnote 
              table.font.size = 16, # Font size table
              heading.title.font.size = 20, # Header size
              table.border.top.color = "transparent", 
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7", 
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% # Stripe rows 
  fmt_number(columns = c(3:4), # Format numbers 
             rows = everything(),
             decimals = 2) %>% 
  data_color(columns = c(Frequency, Avg, SD, Max), # Fill color by value
             fn = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL
             ))

gtsave(relations_tb, "Figures/Relationships.html")

## ---------------------------------- ## 
# Table - Top shareholding companies
## ---------------------------------- ## 

## ------------------------ ## 
# Matches with Colombian Names 
## ----------------------- ## 

# Colombian matches (search terms)
shareholder_comp <- node_relationships %>% 
  filter(origin_match == "1_node_start") %>%  # Filtering for matching-officers
  filter(link == "shareholder of") %>% 
  distinct(node_id_start, node_id_end, link, .keep_all = TRUE) %>% 
  group_by(node_id_end) %>% 
  tally()

# Here we filter for "1_node_start", meaning relationships where matched officers act as the node-start. 
# If we include "2_node_end", we would include all shareholders of matched entities. Meaning shareholders
# that are indirectly associated with matched entities counts as a match, when they are not. 

companies <- shareholder_comp$node_id_end # IDs of companies 

colnames(shareholder_comp) <- c("node_id", "Matches") # Colnames
  
# Extract companies from full data
companies_names <- nodes_entities %>% 
  filter(node_id %in% companies)

names <- companies_names$name

# Total matches 
overall_comp <- relationships %>% 
  filter(node_id_end %in% companies) %>% 
  filter(link == "shareholder of") %>% 
  distinct(node_id_start, node_id_end, link, .keep_all = TRUE) %>% 
  group_by(node_id_end) %>% 
  tally()

colnames(overall_comp) <- c("node_id", "n") # Colnames 

# Merging
top_companies <- merge(shareholder_comp, companies_names, by = "node_id")
top_companies <- merge(top_companies, overall_comp, by = "node_id")

# Final df 
comp_matches <- top_companies %>% 
  dplyr::select(Matches, name, n) %>% 
  mutate(Fraction = Matches/n) %>% 
  arrange(-Matches)

comp_matches$Fraction <- round(comp_matches$Fraction, 3) # Compute fraction 
comp_matches <- comp_matches[, c("name", "n", "Matches", "Fraction")] # Ordering columns
colnames(comp_matches) <- c("Company", "Overall Shareholders", "Colombian Shareholders", "Fraction")

# Table
gt1 <- comp_matches[1:10, ] %>% 
  gt() %>% 
  tab_header(title = "MATCHES WITH COLOMBIAN NAMES") %>% # Title
  opt_all_caps() %>%  # All caps
  tab_source_note(md("**Panel B:** Colombian matches in companies sorted by nr of colombian matches")) %>%  # Footnote
  tab_options(source_notes.font.size = 12, # Sice footnote 
              table.font.size = 14, # Font size table
              heading.title.font.size = 20, # Header size
              table.border.top.color = "transparent", 
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7", 
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% # Stripe rows 
  fmt_number(columns = 4, # Format numbers 
             rows = everything(),
             decimals = 2) %>% 
  data_color(columns = c(Fraction), # Fill color by value
             fn = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL
             ))

## ------------------------ ## 
# Matches with all Names 
## ----------------------- ## 
overall_shareholders <- relationships %>% 
  filter(link == "shareholder of") %>% 
  distinct(node_id_start, node_id_end, link, .keep_all = TRUE) %>%  # Filter for distinct values 
  group_by(node_id_end) %>% 
  tally()

colnames(overall_shareholders) <- c("node_id", "n")

# Merge with df for colombian matches
top_shareholder_comps <- full_join(overall_shareholders, shareholder_comp, by = "node_id")

top_shareholder_comps <- replace(top_shareholder_comps, is.na(top_shareholder_comps), 0) # Replacing NA with 0

# Retrieve company-name, slice top 10
top_10 <- right_join(nodes_entities, top_shareholder_comps, by = "node_id") %>% 
  dplyr::select(Matches, n, name) %>% 
  arrange(-n) %>% 
  filter(Matches > 0) %>% # Filter out companies with no matches
  slice(0:10) # Slice top 10

# Prepare data
top_10 <- top_10[, c("name", "n", "Matches")] # Order columns
top_10$Fraction <- round(top_10$Matches/top_10$n, 4) # Add fraction
colnames(top_10) <- c("Company", "Overall Shareholders", "Colombian Shareholders", "Fraction")

# Table
gt2 <- top_10 %>% 
  gt() %>% 
  tab_header(title = "OVERALL MATCHES") %>% # Title
  opt_all_caps() %>%  # All caps
  tab_source_note(md("**Panel A:** Colombian matches (>0) sorted by nr of shareholders within company")) %>%  # Footnote
  tab_options(source_notes.font.size = 12, # Sice footnote 
              table.font.size = 14, # Font size table
              heading.title.font.size = 20, # Header size
              table.border.top.color = "transparent", 
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7", 
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% # Stripe rows 
  fmt_number(columns = 4, # Format numbers 
             rows = everything(),
             decimals = 2) %>% 
  data_color(columns = c(Fraction), # Fill color by value
             fn = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL
             ))

# -------- # 
# Output   # 
# -------- # 
tables <- list(gt2, gt1)
top_shareholders <- gt_two_column_layout(tables) # Save as image

################################################################
################################################################

## ---------------------------------- ## 
#             Figures
## ---------------------------------- ## 

# Load all matches
match_o <- read.csv("CSV-files/Matches/match_officers.csv")
match_e <- read.csv("CSV-files/Matches/match_entities.csv")

# Search-terms 
last_names <- read.csv("CSV-files/Names/last_names.csv")
last_names <- c(last_names$Name)

# Figure 1 - Histogram of Nr of matched names 
# Maybe a bit tricky way to make the plot.. 

# Officers count
officers_count <- match_o %>% 
  mutate(origin = "1") %>% 
  dplyr::select(term, origin) %>% 
  group_by(term, origin) %>% 
  tally() %>% 
  ungroup()

match <- c(officers_count$term) # Terms with matches 
no_match <- setdiff(last_names, match) # Terms no matches

rows <- tibble(term = no_match, origin = 1, n = 0) # Rows to add 

officers_count <- rbind(officers_count, rows)

# Entities count
entities_count <- match_entities %>% 
  mutate(origin = "2") %>% 
  dplyr::select(term, origin) %>% 
  group_by(term, origin) %>% 
  tally() %>% 
  ungroup()

match <- c(entities_count$term)
no_match <- setdiff(last_names, match)

rows <- tibble(term = no_match, origin = 2, n = 0)

entities_count <- rbind(entities_count, rows)

# All in one df 
all_count <- rbind(officers_count,  entities_count)

# Plot
plot1 <- ggplot(all_count[all_count$n < 95, ], aes(x = n, color = as.factor(origin), 
                                                    fill = as.factor(origin)), group = origin) +
  geom_histogram(bins = 40, position = "dodge") +
  labs(x = "Nr of Matches", y = "Count", title = "Histogram of Nr of Matches") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(size = 15, angle = 0),
        axis.text.y = element_text(size = 15, angle = 0),
        plot.caption = element_text(hjust = 0, size = 8),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15)) +
  scale_y_continuous(breaks = seq(0, 450, 50)) + 
  scale_x_continuous(breaks = seq(0, 95, 10)) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) + 
  scale_color_discrete(labels = c("1" = "Officers", "2" = "Entities")) + 
  scale_fill_discrete(labels = c("1" = "Officers", "2" = "Entities"))
  
# ---------------------------------- # 
# Figure 2 - Officer Activity by year 
# ---------------------------------- # 

# Format years
node_relations_off$start_date_format <- anytime(node_relations_off$start_date) # Extract date format
node_relations_off$start_year <- as.numeric(format(node_relations_off$start_date_format, "%Y")) # Extract year 

node_relations_off$end_date_format <- anytime(node_relations_off$end_date) # Extract date format
node_relations_off$end_year <- as.numeric(format(node_relations_off$end_date_format, "%Y")) # Extract year 

# Transform to long df
long_df <- node_relations_off %>% 
  dplyr::select(start_year, end_year) %>% 
  pivot_longer(cols = c(start_year, end_year), names_to = "Type", values_to = "Year")

# Change names for more neat plot
long_df$Type[long_df$Type == "start_year"] <- "Start Year"
long_df$Type[long_df$Type == "end_year"] <- "End Year"


# Plot
ggplot(long_df, aes(x = Year, fill = Type)) +
  geom_histogram(binwidth = 0.5, position = "stack") +
  labs(x = "Year", y = "Count", title = "Figure ?: Officer Activity by Year ",
       caption = "Sample Footnote") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 15),
        plot.caption = element_text(hjust = 0, size = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_cartesian(xlim = c(1980, 2015))

# Line-plot

# Data
activity <- long_df %>% 
  group_by(Type, Year) %>% 
  tally() %>% 
  filter(Year < 2016 & Year > 1989)

labs <- activity %>% 
  filter(Year %in% c(1990, 1995, 2000, 2005, 2010, 2015))

labs$vjust <- ifelse(labs$Type == "End Year", 1.5, -1.5)

# Colors
colors <- c("End Year" = "#F8766D", "Start Year" = "#00BFC4")

# Plot
plot1 <- ggplot(activity, aes(x = Year, y = n, color = Type)) +
  geom_line() +
  scale_fill_manual(values = colors) +
  coord_cartesian(ylim = c(0, 450),
                  xlim = c(1990, 2015)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_point() + 
  geom_text(data = labs, 
            label = labs$n, vjust = labs$vjust, color = "black", fontface = "bold") +
  ggthemes::theme_fivethirtyeight() + 
  labs(y = "Count", title = "Officer Activity By Year") + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 15),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# ---------------------------------- # 
# Figure 3 - Entity Activity by year 
# ---------------------------------- # 

# Load data 
companies_cl <- read.csv("CSV-files/companies_cl.csv") # Data for connected companies and matching companies

# Format years
match_entities$incorporation_year <- as.Date(match_entities$incorporation_date, "%d-%b-%Y")
match_entities$incorporation_year <- as.numeric(format(match_entities$incorporation_year, "%Y")) # Extract year 

match_entities$inactivation_year <- as.Date(match_entities$inactivation_date, "%d-%b-%Y")
match_entities$inactivation_year <- as.numeric(format(match_entities$inactivation_year, "%Y")) 

match_entities$struck_off_year <- as.Date(match_entities$struck_off_date, "%d-%b-%Y")
match_entities$struck_off_year  <- as.numeric(format(match_entities$struck_off_year, "%Y")) 

# Transform to long df
long_df2 <- companies_cl %>% 
  dplyr::select(incorporation_year, inactivation_year, struck_off_date_year) %>% 
  pivot_longer(cols = c(incorporation_year, inactivation_year, struck_off_date_year), 
               names_to = "Type", values_to = "Year")

# Change names for more neat plot
long_df2$Type[long_df2$Type == "incorporation_year"] <- "Incorporation Year"
long_df2$Type[long_df2$Type == "inactivation_year"] <- "Inactivation Year"
long_df2$Type[long_df2$Type == "struck_off_date_year"] <- "Struck Off Year"

# Plot
ggplot(long_df2, aes(x = Year, fill = Type)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Year", y = "Count") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 15),
        plot.caption = element_text(hjust = 0, size = 8)) + 
  coord_cartesian(xlim = c(1940, 2023)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_cartesian(xlim = c(1960, 2020))

# Line plot 
activity_ent <- long_df2 %>% 
  group_by(Type, Year) %>% 
  tally() %>% 
  filter(Year < 2016 & Year > 1989)

labs_ent <- activity_ent %>% 
  filter(Year %in% c(1990, 1995, 2000, 2005, 2010, 2015))

labs_ent$vjust <- ifelse(labs_ent$Type == "Incorporation Year", -1.5, NA)
labs_ent$vjust <- ifelse(labs_ent$Type == "Inactivation Year", 2, labs_ent$vjust)
labs_ent$vjust <- ifelse(labs_ent$Type == "Struck Off Year", -1.5, labs_ent$vjust)

# Colors
colors <- c("Inactivation Year" = "#F8766D", "Incorporation Year" = "#00BFC4", "Struck Off Year" = "#84749C")

# Plot
plot2 <- ggplot(activity_ent, aes(x = Year, y = n, color = Type)) +
  geom_line() +
  scale_color_manual(values = colors) +
  coord_cartesian(ylim = c(-20, 400),
                  xlim = c(1990, 2015)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_point(size = 1.5) + 
  geom_text(data = labs_ent, 
            label = labs_ent$n, vjust = labs_ent$vjust, color = "black", fontface = "bold") +
  ggthemes::theme_fivethirtyeight() + 
  labs(y = "Count", title = "Entity-Activity By Year") + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 15),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

grid.arrange(plot1, plot2, ncol = 2)

# ------------------------------ # 
# Descriptive tables # 
# ------------------------------ # 

# 1 - Officers # 
# Prepare data 
skim_off <- skim(match_officers) %>% 
  filter(skim_variable %in% c("node_id", "name", "countries", "sourceID", "term"))

n_distinct(match_officers$node_id)

skim_off <- skim_off[c(2:3, 5:8)]

colnames(skim_off) <- c("Variable", "NAs", "Min String Length","Max String Length", "Empty String", "Unique")

skim_off$`Min String Length`[5] <- 3
skim_off$`Max String Length`[5] <- 9
skim_off$`Empty String`[5] <- 0
skim_off$Unique[5] <- 4575

# Table 
skim_off %>% 
  gt() %>% 
  opt_all_caps() %>% 
  tab_options(source_notes.font.size = 12,
              table.font.size = 16,
              heading.title.font.size = 24,
              table.border.top.color = "transparent",
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7",
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% 
  tab_source_note(md("(n = 4881)")) %>% 
  data_color(columns = c(`Max String Length`, `Empty String`, `Unique`), # Fill color by value
             fn = scales::col_numeric(
               palette = c("white", "lightblue", "#3fc1c9"),
               domain = NULL)) 

# 2 - Entities # 
skim_ent <- skim(companies_cl) %>% 
  filter(skim_variable %in% c("name", "jurisdiction", "company_type", "address", "incorporation_date", "inactivation_date",
                              "struck_off_date", "country_codes", "term"))

skim_ent <- skim_ent[c(2:3, 5:8)]

colnames(skim_ent) <- c("Variable", "NAs", "Min String Length","Max String Length", "Empty String", "Unique")


# Table
skim_ent %>% 
  gt() %>% 
  opt_all_caps() %>% 
  tab_options(source_notes.font.size = 12,
              table.font.size = 16,
              heading.title.font.size = 24,
              table.border.top.color = "transparent",
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7",
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% 
  tab_source_note(md("(n = 5680)")) %>% 
  data_color(columns = c(`NAs`, `Empty String`, `Unique`), # Fill color by value
             fn = scales::col_numeric(
               palette = c("white", "lightblue", "#3fc1c9"),
               domain = NULL))


# Groupings
match_officers$continent <- countrycode(match_officers$country_codes, origin = "iso3c", destination = "continent")

match_officers$continent <- ifelse(is.na(match_officers$continent), "Missing", match_officers$continent)

match_officers$continent <- ifelse(match_officers$continent %in% c("Africa", "Asia", "Oceania"), "Rest of The World", match_officers$continent)

continents <- unique(match_officers$continent) # Save as vector 


# ------------------------------ # 
# Descriptive Table 1 - Officers # 
# ------------------------------ # 

# Loop for building descriptive statistics by continent
descriptive_gender  <- list()
descriptive_addresses <- list()
descriptive_relationships <- list()
descriptive_start_date <- list()
descriptive_end_date <- list()

for (i in continents) {
  
  # Filter data 
  continent_data <- match_officers %>% 
    filter(continent == i)
  
  # Gender
  gender <- continent_data %>% 
    group_by(gender) %>% 
    tally() %>% 
    mutate(prop = n/sum(n)*100)
  
  # Addresses
  addresses <- tibble(addresses = c("Total", "Unique", "Missing"),
                      n = c(sum(!is.na(continent_data$address)),
                            n_distinct(continent_data$address),
                            sum(is.na(continent_data$address))))
  
  # Relationships
  continent_rel <- node_relations_off %>% 
    filter(node_id_start %in% c(continent_data$node_id)) %>% 
    filter(rel_type == "officer_of")
  
  continent_rel_stat <- continent_rel %>% 
    group_by(node_id_start) %>% 
    tally()
  
  continent_rel_cl <- tibble(relationships = c("total", "mean", "median", "min", "max"),
                            n = c(nrow(continent_rel), mean(continent_rel_stat$n), median(continent_rel_stat$n),min(continent_rel_stat$n),
                                  max(continent_rel_stat$n)))
  
  # Start date
  start_dates <- continent_rel %>% 
    filter(!is.na(start_year) & !start_year == " ")
  
  
  start_date <- tibble(start_date = c("mean", "min", "max", "median", "missing"),
                       n = c(mean(start_dates$start_year), min(start_dates$start_year), max(start_dates$start_year),
                             median(start_dates$start_year),
                             (nrow(continent_rel) - nrow(start_dates))))
  
  # End date 
  end_dates <- continent_rel %>% 
    filter(!is.na(end_year) & !end_year == " ")
  
  end_date <- tibble(end_date = c("mean", "min", "max", "median", "missing"),
                       n = c(mean(end_dates$end_year), min(end_dates$end_year), max(end_dates$end_year),
                             median(end_dates$end_year),
                             (nrow(continent_rel) - nrow(end_dates))))
  
  # Term 
#  term <- tibble(term = c("unique", "sum"),
         #        n = c(n_unique(continent_data$term), sum(!is.na(continent_data$term))))
  
  # Node_id
 # node_id <- tibble(node_id = c("unique"),
          #          n = n_unique(continent_data$node_id))

  # Save 
  descriptive_gender[[i]] <- gender
  descriptive_addresses[[i]] <- addresses
  descriptive_relationships[[i]] <- continent_rel_cl
  descriptive_start_date[[i]] <- start_date
  descriptive_end_date[[i]] <- end_date

}

descriptive_gender[[4]]
descriptive_addresses[[4]]
descriptive_relationships[[4]]
descriptive_start_date[[4]]
descriptive_end_date[[4]]

# ------------------------------ # 
# Descriptive Table 2 - Entities # 
# ------------------------------ # 
# Groupings
unique(companies_cl$jurisdiction)

companies_cl <- companies_cl %>% 
  mutate(region = case_when(
    jurisdiction %in% c("AW", "BVI", "PMA", "BM", "BRB", "BAH", "VG", "VGB", "CRI", "KY", "VG", "PAN", "CAYMN", "US", "PA",
                        "KNA", "USA", "BLZ", "") ~ "Americas",
    jurisdiction %in% c("XXX", "NEV", "LABUA") ~ "Missing",
    jurisdiction %in% c("MLT", "UK", "JE", "IM") ~ "Europe",
    jurisdiction %in% c("ANG", "SEY", "MU", "SC", "HK", "SGP", "NZL", "SAM", "COOK", "NIUE") ~ "Rest of The World"
  ))


continents <- unique(companies_cl$region) # Save as vector 


# Loop for building descriptive statistics by continent
descriptive_entities  <- list()
descriptive_addresses <- list()
descriptive_incorporation_date <- list()
descriptive_inactivation_date <- list()
descriptive_struck_off_date <- list()


for (i in continents) {
  
  
  # Entities 
  cont_entities <- companies_cl %>% 
    filter(region == "Rest of The World")
  
  ent_data <- tibble(name = c("total", "unique_1", "unique_2"),
                     n = c(nrow(cont_entities), n_distinct(cont_entities$node_id),
                           n_distinct(cont_entities$name)))
  
  # Addresses 
  addresses <- tibble(addresses = c("Total", "Unique", "Missing"),
                      n = c(sum(!is.na(cont_entities$address)),
                            n_distinct(cont_entities$address),
                            sum(is.na(cont_entities$address))))
  
  # Incorporation date
  incorporation_year <- tibble(start_year = c("mean", "min", "max", "median", "missing"),
                               n = c(mean(cont_entities$incorporation_year), min(cont_entities$incorporation_year), 
                                     max(cont_entities$incorporation_year),
                                     median(cont_entities$incorporation_year),
                                     (sum(is.na(cont_entities$incorporation_year)))))
  
  # Inactivation Date
  inact <- cont_entities %>% 
    filter(!is.na(inactivation_year))
  
  inactivation_year <- tibble(end_year = c("mean", "min", "max", "median", "missing"),
                               n = c(mean(inact$inactivation_year), min(inact$inactivation_year), 
                                     max(inact$inactivation_year),
                                     median(inact$inactivation_year),
                                     (sum(is.na(cont_entities$inactivation_year)))))
  
  # Struck Off Date
  struck <- cont_entities %>% 
    filter(!is.na(struck_off_date_year))
  
  
  struck_off_year <- tibble(end_year = c("mean", "min", "max", "median", "missing"),
                              n = c(mean(struck$struck_off_date_year), min(struck$struck_off_date_year), 
                                    max(struck$struck_off_date_year),
                                    median(struck$struck_off_date_year),
                                    (sum(is.na(cont_entities$struck_off_date_year)))))
  
  
  # Saving
  descriptive_entities[[i]]  <- ent_data
  descriptive_addresses[[i]] <- addresses
  descriptive_incorporation_date[[i]] <- incorporation_year
  descriptive_inactivation_date[[i]] <- inactivation_year
  descriptive_struck_off_date[[i]] <- struck_off_year
  
  
}
