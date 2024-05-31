# NHH - Master thesis # 

# 5 - Analysis of entities

# Loading packages 
library(tidyverse)
library(parallel)
library(tictoc)
library(anytime)
library(emojifont)
library(countrycode)
library(ezplot)
library(factoextra)
library(ragg)
library(ggimage)
library(reshape2)
library(png)
library(magick)
library(cowplot)
library(gt)
library(gtExtras)

# Loading data 
# Match-data 
match_entities <- read.csv("CSV-files/Matches/match_entities_cl.csv")
match_officers <- read.csv("CSV-files/Matches/match_officers_cl.csv")

# ICIJ-data
nodes_addresses <- read_csv("ICIJ/Data/nodes-addresses.csv") # Addresses of nodes 
relationships <- read_csv("ICIJ/Data/relationships.csv")     # Relationships between nodes 

nodes_intermediaries <- read.csv("ICIJ/Data/nodes-intermediaries.csv") # Intermediary-nodes
nodes_entities <- read_csv("ICIJ/Data/nodes-entities.csv")             # Entity-nodes 
nodes_officers <- read_csv("ICIJ/Data/nodes-officers.csv")             # Officers-nodes

## -----------------------------------------------## 
### 5) Time Series Visualizations ###
## ---------------------------------------------- ## 

# -------------------------------------------- # 
# 1 - Creating a dataframe of connected entities # 
# -------------------------------------------- # 

companies <- NULL
cl <- makeCluster(6)

tic()

for (i in 1:nrow(match_officers)) {
  
  # Node ID of officer
  node_id <- match_officers$node_id[i]
  term_node <- match_officers$term[i]
  
  # Extract connections to companies
  entity_connections <- relationships %>% 
    filter(node_id_start == node_id) %>% 
    filter(rel_type %in% c("officer_of", "intermediary_of"))
  
  entity_connections <- c(entity_connections$node_id_end)
  
  # Extract connected companies
  companies_con <- nodes_entities %>% 
    filter(node_id %in% entity_connections)
  
  companies_con$term <- term_node
  
  companies <- rbind(companies, companies_con)
  
}

stopCluster(cl)
toc()

# Add origin of match
companies$origin <- "Officer"
match_entities$origin <- "Entity"

# Merge with matched entities 
companies <- rbind(companies, match_entities)

# Cleaning data
companies_cl <- companies %>% 
  distinct(node_id, .keep_all = TRUE) %>% 
  select(node_id, name, jurisdiction, company_type, address, incorporation_date, inactivation_date, 
         dorm_date, status, struck_off_date, country_codes, sourceID, valid_until, term, origin)

# Loop for finding year of current (to which year data is valid)
for (i in 1:nrow(companies_cl)) {
  
  # Extract valid-until column
  valid_until <- companies_cl$valid_until[i]
  
  # Extract last 4 characters of the string
  valid_until <- gsub("[[:punct:]]", "", valid_until) # Remove special characters 
  
  year_valid <- substr(valid_until, nchar(valid_until) - 3, nchar(valid_until))
  
  # Save in column
  companies_cl$valid_until[i] <- year_valid 
  companies_cl$valid_until_year[i] <- paste0(year_valid,"-12-31")
  
}

# Extract relevant years
companies_cl$incorporation_date_form <- anytime(companies_cl$incorporation_date)
companies_cl$incorporation_year <- as.numeric(format(companies_cl$incorporation_date_form, "%Y"))

companies_cl$inactivation_date_form <- anytime(companies_cl$inactivation_date)
companies_cl$inactivation_year <- as.numeric(format(companies_cl$inactivation_date_form, "%Y"))

companies_cl$dorm_date_form <- anytime(companies_cl$dorm_date)
companies_cl$dorm_date_year <- as.numeric(format(companies_cl$dorm_date_form, "%Y"))

companies_cl$struck_off_date_form <- anytime(companies_cl$struck_off_date)
companies_cl$struck_off_date_year <-  as.numeric(format(companies_cl$struck_off_date_form, "%Y"))

# Removing entities with NA for incorporation date
companies_cl <- companies_cl %>% 
  drop_na(incorporation_date)

companies_cl$jurisdiction <- ifelse(companies_cl$jurisdiction %in% c("BVI", "VGB", "VG"), "BVI", companies_cl$jurisdiction)
companies_cl$jurisdiction <- ifelse(companies_cl$jurisdiction %in% c("PAN", "PA"), "PMA", companies_cl$jurisdiction)


# 
companies_cl <- read.csv("CSV-files/Maps_Geocoding/companies_cl2.csv")

# Top jurisdictions 
jur_sum <- companies_cl %>% 
  group_by(jurisdiction) %>% 
  tally() %>% 
  arrange(-n)

colnames(jur_sum) <- c("Jurisdiction", "N") # Colnames

# Prepare for Figure 
jur_sum$iso2 <- countrycode(jur_sum$Jurisdiction, origin = "iso3c", destination = "iso2c") # ISO2 for country-flags 
jur_sum[c(1:2, 3:4, 7, 10:11), ]$iso2 <- c("AW", "VG", "PA", "BM", "BS", "KY", "SC")

jur_sum$Country <- countrycode(jur_sum$iso2, origin = "iso2c", destination = "country.name") # Name for displaying in figure
jur_sum$Continent <- countrycode(jur_sum$iso2, "iso2c", "continent") # Continent (for displaying in figure)
jur_sum$proportion <- round(jur_sum$N/sum(jur_sum$N), 2)
jur_sum$label <- paste0(jur_sum$proportion * 100," ", "%")


# Plot
country_bar <- jur_sum[c(1:7, 9:11), ] %>% 
  ggplot(aes(x = reorder(Country, N), y = N, fill = Continent)) + 
  geom_flag(y = -100, aes(image = iso2), size = 0.1) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = N), hjust = 1.25, size = 4, color = "white", fontface = "bold") +
  geom_text(aes(label = label), hjust = -0.25, size = 7, fontface = "bold", color = "grey40") + 
  labs(x = "Jurisdiction",
       y = "Count") +
  ggthemes::theme_fivethirtyeight() +
  scale_fill_manual(values = c("Americas" = "#f8766d",
                               "Africa" = "#84749c",
                               "Europe" = "#00bfc4")) + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

country_bar + 
  coord_flip() +
  expand_limits(y = -100)


# -------------------------- # 
#  Relations to these entities 
# -------------------------- # 
relationships_comp <- relationships %>% 
  filter(node_id_end %in% c(companies_cl$node_id)) %>% 
  filter(rel_type == "officer_of")

# Find date-format and years of activation
relationships_comp$start_year_format <- anytime(relationships_comp$start_date)
relationships_comp$start_year <- as.numeric(format(relationships_comp$start_year_format, "%Y"))

relationships_comp$end_year_format <- anytime(relationships_comp$end_date)
relationships_comp$end_year <- as.numeric(format(relationships_comp$end_year_format, "%Y"))

# Find valid-date for each relationship (based on type of leak)
cl <- makeCluster(6)
tic()
for (i in 1:nrow(relationships_comp)) {
  
  # Extract type of leak
  leak_type <- relationships_comp$sourceID[i] 
  
  # Extract year of validation
  valid_year <- companies_cl %>% 
    filter(sourceID == leak_type) %>% 
    select(valid_until) %>% 
    distinct()
  
  # Extract valid_year 
  valid_year <- valid_year$valid_until[1]
  
  # Save in df 
  relationships_comp$valid_until[i] <- valid_year
  
  relationships_comp$valid_until_year[i] <- paste0(valid_year,"-12-31")
  
}

stopCluster(cl)
toc()

# Preparing for grouping links
relationships_comp$link <- tolower(relationships_comp$link)
relationships_comp$link_cl <- gsub(" of$", "", relationships_comp$link)

# Creating groups of "links" to entities 
links <- unique(relationships_comp$link_cl)

owner_officers <- c("owner", "ultimate beneficial owner", "beneficial owner", "beneficialowner")

management_officers <- c("president", "managing director", "chairman", "chief financial officer", "board-member", 
                    "chief executive officer", "chairman of the board", "director", "alternate director", "manager", "vice-president")

supporting_officers <- c("auditor", "secretary", "is signatory for")

legal_officers <- c("judicial representative", "legal representative", "appleby assigned attorney", "power of attorney")

other_officers <- links[!links %in% c(owner_officers, management_officers, supporting_officers, legal_officers)]

officer_links <- relationships_comp %>% 
  mutate(group = case_when(
    link_cl == "shareholder" ~ "shareholder",
    link_cl %in% c(owner_officers) ~ "owners",
    link_cl %in% c(management_officers) ~ "management",
    link_cl %in% c(supporting_officers) ~ "support",
    link_cl %in% c(legal_officers) ~ "legal",
    link_cl %in% c(other_officers) ~ "other"
  ))

# Officer-links 
officer_links <- officer_links %>% 
  select(link_cl, group) %>% 
  group_by(group) %>% 
  tally() %>% 
  arrange(-n)

officer_links$prop <- (officer_links$n/sum(officer_links$n))*100

# Colors by group
colors <- c("shareholder" = "#00BFC4", "owners" = "#F8766D", "management" = "#f1dac4", "support" 
            = "#a69cac", "legal" = "#87AFC7", "other" = "#84749C")

# ------------------------------------- # 
# Plot 1: Frequency of officer by groups
# ------------------------------------- # 

barplot <- ggplot(officer_links, aes(x = group, y = n, fill = group)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colors) +
  ggthemes::theme_fivethirtyeight() + 
  labs(x = "Link", y = "Count") +   
  theme(legend.position = "none",
        text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.text = element_text(size = 15)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_text(aes(label = paste0(round(prop, 2), "%")),
            vjust = -0.5, size = 5, fontface = "bold")
  

# Grouping connections
relationships_comp <- relationships_comp %>% 
  mutate(group = case_when(
    link_cl %in% c("shareholder") ~ "shareholder",
    link_cl %in% owner_officers ~ "owners",
    link_cl %in% management_officers ~ "management",
    link_cl %in% supporting_officers ~ "support",
    link_cl %in% legal_officers ~ "legal",
    link_cl %in% other_officers ~ "other"
  ))

# Remove companies with no incorporation-date
companies_cl <- companies_cl %>% 
  filter(!incorporation_date == "")

# Saving for future use
write_csv(companies_cl, "CSV-files/companies_cl.csv")
write_csv(relationships_comp, "CSV-files/relationships_comp.csv")

# Load data 
relationships_comp <- read.csv("CSV-files/Maps_Geocoding/relationships_comp.csv")
company_type <- read.csv("CSV-files/Maps_Geocoding/company_type.csv") # Classification-data

# Adding new cols
companies_cl$name_2 <- company_type$company_name
companies_cl$type <- company_type$company_type
companies_cl$subtype <- company_type$company_subtype

companies_cl <- read.csv("CSV-files/Maps_Geocoding/companies_cl.csv") # Load

# -------------------------------------------------------------------- # 
# Plot 2 and 3  Type and subtype of companies overall vs by jurisdiction
# -------------------------------------------------------------------- # 

# Plot 2 - Overall 
# Subtype
subtype <- companies_cl %>% 
  select(type, subtype) %>% 
  group_by(type, subtype) %>% 
  tally() %>% 
  arrange(-n)

subtype$prop <- (subtype$n/sum(subtype$n))*100

# Color-palette
colors3 <- c("Residential/Commercial" = "#00BFC4", "Contractor" = "#F8766D", "Dealer" = "#f1dac4", "Wholesale Dealer" 
            = "#a69cac", "Food" = "#87AFC7", "Financial Services" = "#84749C")

# Plotting 
barplot2 <- ggplot(subtype[1:6, ], aes(x = subtype, y = n, fill = subtype)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colors3) + 
  ggthemes::theme_fivethirtyeight() + 
  labs(x = "Company Subtype", y = "Count") + 
  theme(legend.position = "none",
        text = element_text(size = 15),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 20),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) + 
  geom_text(aes(label = paste0(round(prop, 2), "%")),
            vjust = -0.5, size = 5, fontface = "bold")

grid.arrange(barplot,  barplot2, ncol = 2)

# Plot 3 - By jurisdiction
# Subtype 
subtype2 <- companies_cl %>% 
  select(jurisdiction, type, subtype) %>% 
  group_by(jurisdiction, type, subtype) %>% 
  tally() %>% 
  arrange(-n) %>% 
  filter(jurisdiction %in% c("AW", "BAH", "BM", "BRB", "BVI", "PMA")) %>% 
  group_by(jurisdiction) %>% 
  mutate(total_n = sum(n)) %>% 
  mutate(percentage = n /total_n * 100) %>% 
  arrange(-percentage)

# Adding labels 
subtype2 <- subtype2 %>% 
  mutate(label = ifelse(jurisdiction == "AW", "🇦🇼 Aruba", jurisdiction),
         label = ifelse(jurisdiction == "BAH", "🇧🇸 Bahamas", label),
         label = ifelse(jurisdiction == "BM", "🇧🇲 Bermuda", label),
         label = ifelse(jurisdiction == "BRB", "🇧🇧 Barbados", label),
         label = ifelse(jurisdiction == "BVI", "🇻🇬 British Virgin Islands", label),
         label = ifelse(jurisdiction == "PMA", "🇵🇦 Panama", label))

# Plotting 
ggplot(subtype2[1:40, ], aes(x = reorder(subtype, percentage), y = percentage, fill = type)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~label) +
  scale_fill_manual(values = colors3) + 
  ggthemes::theme_fivethirtyeight() + 
  labs(x = "Company-Type", y = "Percentage", title = " Highest Shares of Company-types by Jurisdiction") + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8)) +
  theme(strip.text.x = element_text(size = 12, colour = "black", family = "Arial", face = "bold"),
        strip.background = element_rect(fill = "lightblue")) +
  geom_text(aes(label = paste0(round(percentage), "%")),
            hjust = 1.1, size = 3, color = "white", family = "Arial", face = "bold")


# -------------------------------------------- # 
# 2 - Reshaping Data to Panel Data         # 
# -------------------------------------------- # 

# ---------------------- # 
# Building Function 
# ---------------------- # 

# Function for determining active shareholders by year + proportion of Colombians
# Argument 1: Company-relationships, argument 2: Range of years company is active 
entity_profile <- function (rel, years) {
  
  officers_by_year <- NULL # Initiate for saving 
  
  for (i in 1:length(years)) {
    
    # Extract year 
    year <- years[i]
    
    # Creating an interval to check for 
    end_of_year <- paste0(year, "-12-31")
    int <- ymd(end_of_year)
    
    # Counters
    sh_counter <- 0      # Shareholder-counter
    owner_counter <- 0   # Owner-counter
    man_counter <- 0     # Management-counter
    sup_counter <- 0     # Management-support counter
    leg_counter <- 0     # Legal-counter
    other_counter <- 0   # Other officers counter
    col_counter <- 0     # Colombian-counter
    
    for (j in 1:nrow(rel)) {
      
      # Extract nationality 
      nationality <- nodes_officers %>% 
        filter(node_id == rel$node_id_start[j]) %>% 
        select(countries)
      
      nationality <- nationality$countries[1]
      
      # Interval of shareholder start-end-date
      if (is.na(rel$end_year[j])) {
        
        # Start and end of relationship
        start_rel <- rel$start_year_format[j]
        end_rel <- rel$valid_until_year[j]
        
        int2 <- interval(ymd(start_rel), ymd(end_rel))
      } 
      
      # Start - end otherwise
      else {
        # Start and end of relationship
        start_rel <- rel$start_year_format[j]
        end_rel <- rel$end_year_format[j]
        
        # Interval
        int2 <- interval(ymd(start_rel), ymd(end_rel))
      }
      
      # Count nr of active shareholders
      if ((isTRUE(int %within% int2)) & rel$group[j] == "shareholder") {
        
        sh_counter <- sh_counter + 1
      } 
      
      # Count nr of active Colombians
      if ((isTRUE(int %within% int2)) & isTRUE(nationality == "Colombia")) {
        
        col_counter <- col_counter + 1
      }
      
      # Count nr of active owners (owner, beneficial owner)
      if ((isTRUE(int %within% int2)) & rel$group[j] == "owners") {
        
        owner_counter <- owner_counter + 1
      } 
      
      # Count nr of active management officers (including directors)
      if ((isTRUE(int %within% int2)) & rel$group[j] == "management") {
        
        man_counter <- man_counter + 1
      } 
      
      # Count nr of active supporting officers
      if ((isTRUE(int %within% int2)) & rel$group[j] == "support" ) {
        
        sup_counter <- sup_counter + 1
      } 
      
      # Count nr of active legal officers (judicial, attorney etc)
      if ((isTRUE(int %within% int2)) & rel$group[j] == "legal") {
        
        leg_counter <- leg_counter + 1
      } 
    
      # Count nr of other officers
      if ((isTRUE(int %within% int2)) & rel$group[j] == "other") {
        
        other_counter <- other_counter + 1
      }
  }
    
    # Save yearly information 
    nr_shareholders <- tibble(year = year,
                              nr_officers = (sh_counter + owner_counter + man_counter + sup_counter + leg_counter + other_counter),
                              nr_shareholders = sh_counter,
                              nr_owners = owner_counter,
                              nr_management = man_counter,
                              nr_support = sup_counter,
                              nr_legal = leg_counter,
                              nr_others = other_counter,
                              nr_colombians = col_counter)
    
    officers_by_year <- rbind(officers_by_year, nr_shareholders)
  }
  
  # Return df
  return(officers_by_year)
}

# ---------------------- # 
# Building Dataframe 
# ---------------------- # 

# Vector for tax-havens 
tax_havens <- c("AW", "BVI", "PMA", "BM", "BRB", "MLT", "VGB", "BAH", "CAYMN", "SEY")

# Creating data 
entities_descriptive <- NULL # For saving data 

cl <- makeCluster(6) # Make cluster 
tic() # Track time 

# Loop for building dataframe 
for (i in 1:nrow(companies_cl)) {
  
  # Extract node_id of company 
  comp_id <- companies_cl$node_id[i] 
  comp_name <- companies_cl$name[i]
  jur <- companies_cl$jurisdiction[i]
  linked <- companies_cl$country_codes[i]
  
  start_year <- companies_cl$incorporation_year[i]
  end_year <- companies_cl$inactivation_year[i]
  
  # IF-ELSE that determines valid years of incorporation
  if (is.na(end_year)) {
    years <- c(start_year:companies_cl$valid_until[i])
    
    # Start - end otherwise
  } else {
    years <- c(start_year:end_year)
  }

  # Extract relations to this company (where start-date is available)
  comp_relations <- relationships_comp %>% 
    filter(node_id_end == comp_id) %>% 
    filter(!is.na(start_date))
  
  # Skip if no valid relations
  if (nrow(comp_relations) == 0) {
    next
  }
  
  # Initiate function 
  profile <- entity_profile(rel = comp_relations,
                            years = c(years))
  
  # Variable 1 - Tax-haven: 1 if connected to a tax-haven, 0 otherwise
  tax_haven <- ifelse(companies_cl$jurisdiction[i] %in% tax_havens, 1, 0)
  
  # Variable 2 - Colombia-con: 1 if connected to Colombia, 0 otherwise
  colombia_con <- ifelse(companies_cl$country_codes[i] != "COL" | is.na(companies_cl$country_codes[i]), 0, 1) 
  
  # Variable 3 and 4 - Binary-variables by year: 1 if entity was struck off or inactivated, 0 otherwise
  inac <- NULL
  struck_o <- NULL
  
  for (j in 1:length(years)) {
    
    year <- years[j]
    
    # Year of inactivation
    if (!is.na(companies_cl$inactivation_date[i])) {
      inac[j] <- ifelse(companies_cl$inactivation_year[i] == year, "1", "0")
      
    } else {
      inac[j] <- 0
    }
    
    # Year struck-off
    if (!is.na(companies_cl$struck_off_date_year[i])) {
      struck_o[j] <- ifelse(companies_cl$struck_off_date_year[i] == year, "1", "0")
    
    } else {
      struck_o[j] <- 0
    }
    
  }

  # Save further variables in another df
  profile2 <- tibble(
                     entity_id = rep(comp_id, nrow(profile)),
                     entity_name = rep(comp_name, nrow(profile)),
                     jurisdiction = rep(jur, nrow(profile)),
                     tax_haven = rep(tax_haven, nrow(profile)),
                     linked_country = rep(linked, nrow(profile)),
                     colombia_con = rep(colombia_con, nrow(profile)),
                     inactivation = c(inac),
                     struck_off = c(struck_o))
  
  # Create full-profile 
  profile <- cbind(profile2, profile)
  
  # Add to initiated dataframe
  entities_descriptive <- rbind(entities_descriptive, profile)

}

stopCluster(cl) # Stop cluster 
toc() # Track time 

# Add company_type and sub_type 
for (i in 1:nrow(entities_descriptive)) {
  
  # Extract name
  name <- entities_descriptive$entity_name[i]
  
  # Extract subtype and type
  types <- company_type %>% 
    filter(company_name == name) %>% 
    select(company_type, company_subtype)
  
  # Add to row
  entities_descriptive$company_type[i] <- types$company_type[1]
  entities_descriptive$company_subtype[i] <- types$company_subtype[1]
  
}

write_csv(entities_descriptive, "CSV-files/entities_descriptive2.csv") # Save Panel Data 

entities_descriptive <- read.csv("CSV-files/Panel-Data/entities_descriptive2.csv") # Load

# -------------------------- # 
#  Time-Series Analysis 
# -------------------------- # 

# Summarize unique terms by year and jurisdiction
entities_descriptive$jurisdiction <- ifelse(entities_descriptive$jurisdiction %in% c("VGB", "VG"), "BVI", entities_descriptive$jurisdiction)
entities_descriptive$jurisdiction <- ifelse(entities_descriptive$jurisdiction %in% c("PA", "PAN"), "PMA", entities_descriptive$jurisdiction)
entities_descriptive$jurisdiction <- ifelse(entities_descriptive$jurisdiction %in% c("CAYMN", "KY"), "CAYMN", entities_descriptive$jurisdiction)
entities_descriptive$jurisdiction <- ifelse(entities_descriptive$jurisdiction %in% c("SC"), "SEY", entities_descriptive$jurisdiction)

# -------------------------------------------------------------- # 
# Plot 1 - Unique search-terms connected to jurisdictions by year
# -------------------------------------------------------------- # 

# Loop for adding search-terms to entities-profile
for (i in 1:nrow(entities_descriptive)) {
  
  # Extract node-id
  node <- entities_descriptive$entity_id[i]
  
  # Look up connected search-term and origin
  company <- companies_cl %>% 
    filter(node_id == node)
  
  # Add to df 
  entities_descriptive$term[i] <- company$term[1]
  entities_descriptive$origin[i] <- company$origin[1]

}

tax_havens <- c("AW", "BM", "BAH", "BVI", "BRB", "PMA")
tax_havens2 <- c("CAYMN", "COOK", "SEY", "NEV", "SAM", "ANG")

terms_sum <- terms_sum %>% 
  group_by(year) %>% 
  mutate(total_terms = sum(unique_terms))

terms_sum <- entities_descriptive %>% 
 # filter(!entity_id %in% c(comp_off$node_id)) %>% 
  select(jurisdiction, year, term) %>% 
  group_by(year) %>% 
  mutate(total_terms = n_distinct(term)) %>% 
  filter(year < 2016,
         year > 1989) %>% 
  group_by(jurisdiction, year) %>% 
  summarise(unique_terms = n_distinct(term))

terms_sum <- terms_sum %>% 
  group_by(year) %>% 
  mutate(total_terms = sum(unique_terms))

terms_sum$proportion <- terms_sum$unique_terms/terms_sum$total_terms

terms_sum1 <- terms_sum %>% 
  filter(jurisdiction %in% tax_havens2) 

# For tax havens 1 
# Create a new df for plotting flags at the end of each line
end <- terms_sum1 %>% 
  filter(year == 2015 & !jurisdiction == "BM" | year == 2014 & jurisdiction == "BM")

end$iso2 <- c("AW", "BS", "BM", "BB", "VG" ,"PA") # Add iso2-code
end$country <- c("Aruba", "Bahamas", "Bermuda", "Barbados", "British Virgin Islands", "Panama")

# For tax-havens 2 
end <- terms_sum1 %>% 
  filter(year == 2015 | year == 2014 & jurisdiction == "CAYMN" )

end$iso2 <- c("AO", "KY", "CK", "NZ", "AS", "SC")

# Plotting
ggplot(terms_sum1, aes(x = year, y = proportion)) + 
  geom_line(aes(color = jurisdiction)) +
  geom_flag(data = end, x = end$year, aes(image = iso2), size = 0.06) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  ggthemes::theme_fivethirtyeight() + 
  coord_cartesian(xlim = c(1990, 2015)) +
  labs(x = "Year", y = "Nr of Unique Terms", title = "Unique Search-Terms by Jurisdiction") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
  scale_color_manual(values = c("#418fde", "#00a9ce", "purple", "#FFC726", "#00205b", "#DA121A"),
                     labels = c("Aruba 🇦🇼", "Bahamas 🇧🇸", "Bermuda 🇧🇲", "Barbados 🇧🇧", 
                                "British Virgin Islands 🇻🇬", "Panama 🇵🇦")) 

# Plotting
ggplot(terms_sum1, aes(x = year, y = proportion)) + 
  geom_line(aes(color = jurisdiction)) +
  geom_flag(data = end, x = end$year, aes(image = iso2), size = 0.075) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  ggthemes::theme_fivethirtyeight() + 
  coord_cartesian(xlim = c(1990, 2015)) +
  labs(x = "Year", y = "Nr of Unique Terms", title = "Unique Search-Terms by Jurisdiction") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
  scale_color_manual(values = c("#418fde", "#00a9ce", "purple", "#FFC726", "#00205b", "#DA121A"),
                     labels = c("Angola 🇦🇴", "Cayman Islands 🇰🇾", "Cook Islands 🇨🇰", "New Zealand 🇳🇿", 
                                "American Samoa 🇦🇸", "Seychelles 🇸🇨")) 
  
# -------------------------------------------------------------- # 
# Plot - Average nr of officer-groups by jurisdiction
# -------------------------------------------------------------- # 

# Calculate avg nr of officers by year and jurisdiction
avg_officers_jur <- entities_descriptive %>% 
  filter(nr_officers > 0 & nr_officers < 100) %>% 
  select(jurisdiction, year, nr_officers, nr_shareholders, nr_owners, nr_management, nr_support, nr_legal, nr_others) %>% 
  group_by(jurisdiction, year) %>% 
  summarise(count = n(), # Nr of observations by year 
            avg_nr_officers = mean(nr_officers, na.rm = TRUE),
            avg_nr_shareholders = mean(nr_shareholders, na.rm = TRUE),
            avg_nr_owners = mean(nr_owners, na.rm = TRUE),
            avg_nr_management = mean(nr_management, na.rm = TRUE),
            avg_nr_support = mean(nr_support, na.rm = TRUE),
            avg_nr_legal = mean(nr_legal, na.rm = TRUE),
            avg_nr_others = mean(nr_others, na.rm = TRUE)) %>% 
  filter(year < 2016 & year > 1989) %>% 
  filter(jurisdiction %in% c("AW", "BAH", "BM", "BRB", "PMA", "BVI"))

# Proportion
prop_officers_jur <- entities_descriptive %>%
  filter(nr_officers > 0 & nr_officers < 100) %>%
  select(jurisdiction, year, nr_officers, nr_shareholders, nr_owners, nr_management, nr_support, nr_legal, nr_others) %>%
  group_by(jurisdiction, year) %>%
  summarise(count = n(), # Nr of observations by year 
            total_nr_officers = sum(nr_officers, na.rm = TRUE),
            total_nr_shareholders = sum(nr_shareholders, na.rm = TRUE),
            total_nr_owners = sum(nr_owners, na.rm = TRUE),
            total_nr_management = sum(nr_management, na.rm = TRUE),
            total_nr_support = sum(nr_support, na.rm = TRUE),
            total_nr_legal = sum(nr_legal, na.rm = TRUE),
            total_nr_others = sum(nr_others, na.rm = TRUE)) %>%
  mutate(prop_nr_officers = total_nr_officers / total_nr_officers,
         prop_nr_shareholders = total_nr_shareholders / total_nr_officers,
         prop_nr_owners = total_nr_owners / total_nr_officers,
         prop_nr_management = total_nr_management / total_nr_officers,
         prop_nr_support = total_nr_support / total_nr_officers,
         prop_nr_legal = total_nr_legal / total_nr_officers,
         prop_nr_others = total_nr_others / total_nr_officers) %>%
  filter(year < 2016 & year > 1989) %>%
  filter(jurisdiction %in% c("AW", "BAH", "BM", "BRB", "PMA", "BVI"))

# Data for flags
flag_df <- tibble(country = c("🇦🇼 Aruba", "🇧🇸 Bahamas", "🇧🇲 Bermuda", "🇧🇧 Barbados", "🇻🇬 British Virgin Islands", "🇵🇦 Panama"))

avg_officers_jur <- prop_officers_jur %>% 
  mutate(label = ifelse(jurisdiction == "AW", "🇦🇼 Aruba", jurisdiction),
         label = ifelse(jurisdiction == "BAH", "🇧🇸 Bahamas", label),
         label = ifelse(jurisdiction == "BM", "🇧🇲 Bermuda", label),
         label = ifelse(jurisdiction == "BRB", "🇧🇧 Barbados", label),
         label = ifelse(jurisdiction == "BVI", "🇻🇬 British Virgin Islands", label),
         label = ifelse(jurisdiction == "PMA", "🇵🇦 Panama", label))

colors <- c("Officers" = "blue", "Shareholders" = "red", "Owners" = "purple", "Management" = "pink", "Support" 
            = "darkgreen", "Legal" = "cyan", "Others" = "grey40")

# Plotting
ggplot(avg_officers_jur, aes(x = year)) +
  facet_wrap(~ label, scales = "free_y") + 
#  geom_line(aes(y = prop_nr_officers, color = "Officers"), lty = "dashed") +
 # geom_smooth(aes(y = prop_nr_officers), method = "lm", linewidth = 0.3, lty = "dotted", color = "black") + 
  geom_line(aes(y = prop_nr_shareholders, color = "Shareholders"), lty = "solid") +
  geom_line(aes(y = prop_nr_owners, color = "Owners"), lty = "solid") +
  geom_line(aes(y = prop_nr_management, color = "Management"), lty = "solid") +
  geom_line(aes(y = prop_nr_support, color = "Support"), lty = "solid") +
  geom_line(aes(y = prop_nr_legal, color = "Legal"), lty = "solid") +
  geom_line(aes(y = prop_nr_others, color = "Others"), lty = "solid") +
  ggthemes::theme_fivethirtyeight() +
#  theme_bw() + 
  labs(x = "Year", y = "Average", title = "Proportion of Officers by Jurisdiction (1990 - 2015) ") + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  coord_cartesian(xlim = c(1990, 2014)) + 
  scale_color_manual(values = colors) + 
  theme(strip.text.x = element_text(size = 13, colour = "black", family = "Arial", face = "bold"),
        strip.background = element_rect(fill = "lightblue")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 100))

# ------------------------------------------------------------------- # 
# Plot - Avg Nr of Officers by Company-type by Jurisdiction
# ------------------------------------------------------------------ # 
# Create df 
comp_avg <- entities_descriptive %>% 
  filter(year < 2015 & year > 1989 & jurisdiction %in% c("AW", "BRB", "BM", "BAH", "PMA", "BVI") 
         & company_subtype %in% c("Residential/Commercial", "Contractor", "Dealer", "Wholesale Dealer", "Food", "Financial Services")) %>% 
  select(year, jurisdiction, company_subtype, nr_officers) %>% 
  filter(nr_officers > 0 & nr_officers < 100) %>% 
  group_by(year, jurisdiction, company_subtype) %>% 
  summarise(avg_officer = mean(nr_officers, na.rm = TRUE)) %>% 
  mutate(label = ifelse(jurisdiction == "AW", "🇦🇼 Aruba", jurisdiction),
         label = ifelse(jurisdiction == "BAH", "🇧🇸 Bahamas", label),
         label = ifelse(jurisdiction == "BM", "🇧🇲 Bermuda", label),
         label = ifelse(jurisdiction == "BRB", "🇧🇧 Barbados", label),
         label = ifelse(jurisdiction == "BVI", "🇻🇬 British Virgin Islands", label),
         label = ifelse(jurisdiction == "PMA", "🇵🇦 Panama", label))

# Proportion
comp_prop <- entities_descriptive %>% 
  filter(year < 2015 & year > 1989 & jurisdiction %in% c("AW", "BRB", "BM", "BAH", "PMA", "BVI")) %>% 
#  select(year, jurisdiction, company_subtype, nr_officers) %>% 
  filter(nr_officers > 0 & nr_officers < 100) %>% 
  group_by(year, jurisdiction, company_subtype) %>% 
  summarise(tot_officers = sum(nr_officers, na.rm = TRUE)) %>% 
  group_by(year, jurisdiction) %>%
  mutate(prop_nr_officers = tot_officers / sum(tot_officers)) %>%
  ungroup() %>% mutate(label = ifelse(jurisdiction == "AW", "🇦🇼 Aruba", jurisdiction),
         label = ifelse(jurisdiction == "BAH", "🇧🇸 Bahamas", label),
         label = ifelse(jurisdiction == "BM", "🇧🇲 Bermuda", label),
         label = ifelse(jurisdiction == "BRB", "🇧🇧 Barbados", label),
         label = ifelse(jurisdiction == "BVI", "🇻🇬 British Virgin Islands", label),
         label = ifelse(jurisdiction == "PMA", "🇵🇦 Panama", label)) %>% 
  filter(company_subtype %in% c("Residential/Commercial", "Contractor", "Dealer", "Food", "Financial Services"))

# Colors
colors3 <- c("Residential/Commercial" = "blue", "Contractor" = "red", "Dealer" = "purple",  "Food" 
            = "grey20", "Financial Services" = "grey30")

# Plotting
ggplot(comp_prop, aes(x = year, y = prop_nr_officers, group = company_subtype, color = company_subtype)) +
  geom_line() + 
  facet_wrap(~label, scales = "free_y") + 
  ggthemes::theme_fivethirtyeight() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  labs(x = "Year", y = "Average Nr of Officers", title = "Proportion of Officers By Company-Type (1990-2015)") +
  theme(legend.title = element_blank(),
                 legend.position = "bottom",
                 plot.caption = element_text(hjust = 0, size = 8),
                 plot.background = element_rect(fill = "white"),
                 panel.background = element_rect(fill = "white")) +
   #  coord_cartesian(xlim = c(1990, 2014)) + 
     scale_color_manual(values = colors3) + 
     theme(strip.text.x = element_text(size = 13, colour = "black", family = "Arial", face = "bold"),
                      strip.background = element_rect(fill = "lightblue")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 100))

# ---------------------------------------------------------------- # 
# -------------------- Descriptive Statistics  ------------------- #
# ---------------------------------------------------------------- # 

# Longevity
longevity <- entities_descriptive%>% 
  group_by(entity_id) %>% 
  summarise(count = n())

mean(longevity$count)
median(longevity$count)
sd(longevity$count)
min(longevity$count)
max(longevity$count)

# Year
mean(entities_descriptive$year)
median(entities_descriptive$year)
sd(entities_descriptive$year)
min(entities_descriptive$year)
max(entities_descriptive$year)

# Officers, shareholders, management etc. 
mean(entities_descriptive$nr_officers)
median(entities_descriptive$nr_officers)
sd(entities_descriptive$nr_officers)
min(entities_descriptive$nr_officers)
max(entities_descriptive$nr_officers)


# ------------------------------------------------- # 
# Plot -  Avg nr Officers Relative to Shocks
# ------------------------------------------------ # 

# Aid and Cultivation
col_aid <- read.csv("CSV-files/Coca_Data/col_aid.csv")
coca_crops <- read.csv("CSV-files/Coca_Data/coca_crops.csv")

# Avg officers
avg_officers <- entities_descriptive %>% 
  filter(nr_officers > 0 & nr_officers < 50) %>% 
  select(jurisdiction, year, nr_officers, nr_shareholders, nr_owners, nr_management, nr_support, nr_legal, nr_others) %>% 
  group_by( year) %>% 
  summarise(count = n(), # Nr of observations by year 
            avg_nr_officers = mean(nr_officers, na.rm = TRUE),
            avg_nr_shareholders = mean(nr_shareholders, na.rm = TRUE),
            avg_nr_owners = mean(nr_owners, na.rm = TRUE),
            avg_nr_management = mean(nr_management, na.rm = TRUE),
            avg_nr_support = mean(nr_support, na.rm = TRUE),
            avg_nr_legal = mean(nr_legal, na.rm = TRUE),
            avg_nr_others = mean(nr_others, na.rm = TRUE)) %>% 
  filter(year < 2016,
         year > 1989)

# Plot 
ggplot() +
  geom_line(data = col_aid, aes(y = scale(aid_millions), x = Fiscal.Year, color = "US Aid"), lty = "longdash") + 
  geom_line(data = coca_crops, aes(y = scale(cultivation_hectares), x = year, color = "Coca Crops"), lty = "dashed") + 
  geom_line(data = avg_officers, aes(y = scale(avg_nr_officers), x = year, color = "Officers")) + 
  
  # Theme and labs
  ggthemes::theme_fivethirtyeight() +
  labs(x = "Year", y = "Scaled Values", title = "Entity-Development Relative to Shocks") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
         axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_color_manual(values = c("red", "darkblue", "darkgreen"),
                     labels = c("Cocaine Crops",  "Average Nr Officers", "US Aid")) +
  xlim(1990, 2014)
