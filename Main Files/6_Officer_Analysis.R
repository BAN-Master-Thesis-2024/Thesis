# NHH - Master Thesis # 

# 6 - Analysis of Officers

# Loading packages 
library(tidyverse)
library(parallel)
library(tictoc)
library(anytime)
library(emojifont)
library(ggimage)

# Loading data 
match_officers <- read.csv("CSV-files/Matches/match_officers2.csv")

# ICIJ-data
nodes_addresses <- read_csv("ICIJ/Data/nodes-addresses.csv") # Addresses of nodes 
relationships <- read_csv("ICIJ/Data/relationships.csv")     # Relationships between nodes 

nodes_intermediaries <- read.csv("ICIJ/data/nodes-intermediaries.csv") # Intermediary-nodes
nodes_entities <- read_csv("ICIJ/Data/nodes-entities.csv")             # Entity-nodes 
nodes_officers <- read_csv("ICIJ/Data/nodes-officers.csv")             # Officers-nodes


# Note:

## -----------------------------------------------## 
### 6) Analysis of changes for matched officers ###
## ---------------------------------------------- ## 

# ------------- # 
# Data Wrangling # 
# ------------- # 

# Retrieve all relationships for officers 
rel_off <- relationships %>% 
  filter(node_id_start %in% c(match_officers$node_id)) %>% 
  filter(rel_type == "officer_of") %>% # Only keep officer observations 
  filter(!is.na(start_date)) %>% 
  mutate(valid_until = "",
         entity = "",
         jurisdiction = "",
         linked_country = "",
         term = "",
         gender = "",
         country = "",
         name = "")

# Retrieve validation-year for data and additional data for each entity 

cl <- makeCluster(6)

for (i in 1:nrow(rel_off)) {
  
  # Extract node-id for entity
  node <- rel_off$node_id_end[i]
  
  # Extract entity
  ent <- nodes_entities %>% 
    filter(node_id == node)
  
  # Extract node-id for match
  node2 <- rel_off$node_id_start[i]
  
  # Extract match
  match <- match_officers %>% 
    filter(node_id == node2)
  
  # Entity data 
  rel_off$entity[i] <- ent$name[1]
  rel_off$jurisdiction[i] <- ent$jurisdiction[1]
  rel_off$linked_country[i] <- ent$country_codes[1]
  
  # Officer data
  rel_off$name[i] <- match$name[1]
  rel_off$country[i] <- match$country_codes[1]
  rel_off$term[i] <- match$term[1]
  rel_off$gender[i] <- match$gender[1]
  
  # Extract validation
  rel_off$valid_until[i] <- ent$valid_until[1]
  
  rel_off$valid_until[i] <- gsub("[[:punct:]]", "", rel_off$valid_until[i]) # Remove special characters 
  rel_off$valid_until[i] <- substr(rel_off$valid_until[i], nchar(rel_off$valid_until[i]) - 3, nchar(rel_off$valid_until[i]))
  
  # Insert valid-year 
  if (is.na(rel_off$end_date[i])) {
    
    rel_off$end_date[i] <- paste0(rel_off$valid_until[i], "-12-31")
    
  }
}

stopCluster(cl)

# Format years
rel_off$start_year_form <- anytime(rel_off$start_date)
rel_off$start_year <- as.numeric(format(rel_off$start_year_form, "%Y"))

rel_off$end_year_form <- anytime(rel_off$end_date)
rel_off$end_year <- as.numeric(format(rel_off$end_year_form, "%Y"))

# ------------------------------------ # 
# Function for creating officer-profile # 
# ------------------------------------ # 

officer_profile <- function (years, rel) {
  
  officers_by_year <- NULL # Initiate 
  
  # Counter
  years_active <- 0
  
  for (i in 1:length(years)) {
    
    # Extract year 
    year <- years[i]
    
    # Creating an interval to check for 
    end_of_year <- paste0(year, "-12-31")
    int <- ymd(end_of_year)
    
    # Interval to check for 
    start_rel <- rel$start_year_form[1]
    end_rel <- rel$end_year_form[1]
      
    int2 <- interval(ymd(start_rel), ymd(end_rel))
      
    # Check if officer is active
    if ((isTRUE(int %within% int2))) {
        
      active <- 1 # Binary variable (active or not)
      years_active <- years_active + 1
        
    } else {
        active <- 0
        years_active <- years_active
    }
    
    # Save data 
    officer_prof <- tibble(node_id = rel$node_id_start[1],
                           name = rel$name[1],
                           country = rel$country[1],
                           linked_to = rel$entity[1],
                           link = rel$link[1],
                           jurisdiction = rel$jurisdiction[1],
                           year = year,
                           active = active,
                           yr_active = years_active,
                           gender = rel$gender[1],
                           term = rel$term[1])
    
    officers_by_year <- rbind(officer_prof, officers_by_year)
    
    }
    
  return(officers_by_year)
  
  }

officer_profile(years = c(rel_off$start_year[1]:rel_off$end_year[1]),
                rel = rel_off[1, ])


# ---------------------- # 
# Building Dataframe 
# ---------------------- # 

rel_off <- rel_off %>% 
  filter(!is.na(start_year) & !is.na(end_year))

officers_descriptive <- NULL

cl <- makeCluster(6) # Make cluster 
tic() # Track time 

for (i in 1:nrow(rel_off)) {
  
  # Variables to account for 
  years <- c(rel_off$start_year[i]:rel_off$end_year[i])
  rel <- rel_off[i, ]
  
  profile <- officer_profile(years = years, rel = rel)
  
  # Add to df
  officers_descriptive <- rbind(officers_descriptive, profile)
  
}

stopCluster(cl)

# Save as CSV 
write_csv(officers_descriptive, "CSV-files/officers_descriptive2.csv") 

officers_descriptive <- read.csv("CSV-files/Panel-Data/officers_descriptive2.csv")

# Fixing names
officers_descriptive$jurisdiction <- ifelse(officers_descriptive$jurisdiction %in% c("PAN", "PMA", "PA"), "PMA", 
                                            officers_descriptive$jurisdiction)

officers_descriptive$jurisdiction <- ifelse(officers_descriptive$jurisdiction %in% c("VG", "VGB"), "BVI", 
                                            officers_descriptive$jurisdiction)


officers_descriptive$jurisdiction <- ifelse(officers_descriptive$jurisdiction %in% c("KY", "CAYMN"), "CAYMN", 
                                            officers_descriptive$jurisdiction)

# ----------------------------------- # 
# Plot 1 - Activity by Gender by year 
# ----------------------------------- # 
activity <- officers_descriptive %>% 
  filter(!active == 0) %>% 
  filter(!gender == "NA") %>% 
  filter(jurisdiction %in% c("AW")) %>% 
  #filter(jurisdiction %in% c("AW", "BVI", "PMA", "BM", "BAH", "BRB")) %>% 
  distinct(node_id, year, jurisdiction, gender) %>% 
  group_by(year, gender) %>% 
  tally() %>% 
  filter(year < 2015 & year > 1989) %>% 
  group_by(year) %>% 
  mutate(total = sum(n)) %>% 
  group_by(year, gender) %>% 
  mutate(prop = n/total) 

activity_labs <- activity %>% 
  filter(year %in% c(1990, 1995, 2000, 2005, 2010, 2015))

activity_labs$vjust <- ifelse(activity_labs$gender == "male", -1.5, 1.5)
  
# Plot
ggplot(activity, aes(x = year, y = prop , color = gender)) + 
  geom_line() +
  geom_point(size = 1.5) + 
  geom_text(data = activity_labs,
            label = paste0(round(activity_labs$prop * 100), "%"), vjust = activity_labs$vjust, size = 3.5, color = "black",
           fontface = "bold") + 
  coord_cartesian(xlim = c(1990, 2015),
                  ylim = c(0, 1.0)) +
  ggthemes::theme_fivethirtyeight() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 15)) +
  labs(x = "Year", y = "Proportion", title = "Gender Proportion for Common Tax Havens (1990 - 2015)")

# ----------------------------------- # 
#  Nr of males per female By Jurisdiction
# ----------------------------------- # 
male_female <- officers_descriptive %>% 
  filter(!active == 0) %>% 
  filter(!gender == "NA") %>% 
 # filter(jurisdiction %in% c("AW", "BVI", "PMA", "BM", "BAH", "BRB")) %>% 
  distinct(node_id, year, jurisdiction, gender) %>% 
  group_by(year, jurisdiction, gender) %>% 
  tally() %>% 
  filter(year > 1989 & year < 2016) %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate(male_female_ratio = male/female) %>% 
  filter(jurisdiction %in% c("AW", "BRB", "BAH", "BM", "PMA", "BVI")) %>% 
  group_by(year, jurisdiction) %>% 
  filter(sum(female + male) > 20)

# Add 0 in cases with no females
male_female$male_female_ratio <- ifelse(is.na(male_female$male_female_ratio), 0, male_female$male_female_ratio)

# Plotting
ggplot(male_female, aes(x = year, y = male_female_ratio, color = jurisdiction)) + 
  geom_line() + 
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 15))


# ------------------------------- # 
# Plot 2 - Activity by year 
# ------------------------------- # 
activity_yr <- officers_descriptive %>% 
  filter(!active == 0 & jurisdiction == "AW") %>% 
  group_by(year) %>% 
  tally()

activity_yr <- officers_descriptive %>% 
  distinct(year, jurisdiction, term) %>% 
  group_by(year, jurisdiction) %>% 
  tally()

# ------------------------------- # 
# Plot 2 - Share of Colombian by Year 
# ------------------------------- # 

# Add binary for col
officers_descriptive$colombian <- ifelse(officers_descriptive$country == "COL", 1, 0)

col_year <- officers_descriptive %>% 
  group_by(year, node_id) %>% 
 # filter(n() < 6) %>% 
 # filter(!is.null(country)) %>% 
 # filter(!country == "") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year > 1989 & year < 2016) %>% 
  summarise(tot_officers = n(),
            south_america_officers = sum(country %in% c( "ARG", "PER", "URY", "ECU", "VEN", "CHL", "BRA", "PRY", "BOL")),
            north_america_officers = sum(country %in% c("USA", "CAN")),
            latin_america_officers = sum(country %in% c("PAN", "MEX", "GTM", "SLV", "CRI", "NIC")),
            europe_officers = sum(country %in% c("JEY", "ESP", "AND", "GBR", "FRA", "RUS", "MLT", "ITA", "SWE", "CHE", "GIB", "GGY", "LIE",
                                                 "IRL")),
            africa_officers = sum(country %in% c("CPV", "SYC", "ZAF", "LBR", "EGY")),
            asia_officers = sum(country %in% c("HKG;PHL", "PHL", "THA", "CHN", "ARE", "HKG","IDN", "AUS", "ISR", "LAO", "TWN", "NRU",
                                               "QAT", "LBN")),
            tax_havens_officers = sum(country %in% c("JEY", "BMU;GBR", "BMU", "VGB", "BHS", "CYM", "DOM", "MLT", "DOM;GTM", "PAN")),
            col_officers = sum(country %in% c("COL"))
            
            ) %>% 
  mutate(south_share = (south_america_officers/tot_officers)*100,
         north_share = (north_america_officers/tot_officers)*100,
         latin_share = (latin_america_officers/tot_officers)*100,
         europe_share = (europe_officers/tot_officers)*100,
         africa_share = (africa_officers/tot_officers)*100,
         asia_share = (asia_officers/tot_officers)*100,
         tax_share = (tax_havens_officers/tot_officers)*100,
         col_share = (col_officers/tot_officers)*100)


# Plot
ggplot() +
  geom_line(data = col_aid, aes(y = scale(aid_millions), x = Fiscal.Year, color = "US Aid"), lty = "longdash") + 
  geom_line(data = coca_crops, aes(y = scale(cultivation_hectares), x = year, color = "Coca Crops"), lty = "dashed") + 
  geom_line(data = col_year, aes(y = scale(tax_share), x = year, color = "Caribbean-Haven Link")) +
  
  # Theme and labs
  ggthemes::theme_fivethirtyeight() +
  labs(x = "Year", y = "Scaled Variables", title = "Officer-Development Relative to Shocks") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
       # axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white")) +
  scale_color_manual(values = c("darkblue", "red","darkgreen"),
                     labels = c( "Tax-Haven Link",  "Cocaine Crops","US Aid")) +
  xlim(1990, 2014)
  
# ----------------------------------------- # 
# Plot 3 - Count of Active Officers by Jurisdiction
# ---------------------------------------- # 
# Activity by jurisdiction
jur_activity <- 
  officers_descriptive %>% 
  filter(!active == 0) %>%  # Remove inactive 
  distinct(node_id, jurisdiction, year) %>%  # Distinct connections to same entity 
  group_by(jurisdiction, year) %>% 
  tally() %>% 
  group_by(year) %>% 
  mutate(total = sum(n)) %>%  # Sum by year 
  group_by(year, jurisdiction) %>% 
  mutate(share = n /total) %>%  # Share 
  filter(year > 1989, year < 2016) %>%  # Filter for period between 1989 and 2015
  filter(jurisdiction %in% c("AW", "PMA", "BAH", "BM", "BVI", "BRB")) # Filter for main havens

# Flag
jur_flag <- jur_activity %>% 
  filter(year %in% c(2014:2015))

jur_flag <- jur_flag[-c(1, 3, 6, 8, 10), ]

jur_flag$iso2 <- c("AW", "BS", "BM","BB", "VG", "PA")

# Plot
ggplot(data = jur_activity, aes(x = year, y = share)) + 
  geom_line(aes(color = jurisdiction)) +
  geom_flag(data = jur_flag, 
            aes(x = year, y = share, image = iso2), size = 0.08, position = position_dodge(width = 1)) +
  labs(x = "Year", y = "Share", title = "Share of Active Officers by Jurisdiction") +
  coord_cartesian(xlim = c(1990, 2015)) +
  ggthemes::theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
       # axis.title.x = element_text(face = "bold", size = 15),
       # axis.title.y = element_text(face = "bold", size = 15),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
  scale_color_manual(values = c("#418fde", "#00a9ce", "darkgreen", "#FFC726","#00205b", "#DA121A"),
                     labels = c("AW 🇦🇼", "BAH 🇧🇸", "BM 🇧🇧","BRB 🇧🇧", "BVI 🇻🇬", "PMA 🇵🇦"))


# ---------------------------------------------------------------- # 
# -------------------- Descriptive Statistics  ------------------- #
# ---------------------------------------------------------------- # 

officers_descriptive$gender_dummy <- ifelse(officers_descriptive$gender == "male", 1, 0)

# Longevity
mean(officers_descriptive$yr_active)
median(officers_descriptive$yr_active)
sd(officers_descriptive$yr_active)
min(officers_descriptive$yr_active)
max(officers_descriptive$yr_active)

# Year
mean(officers_descriptive$year)
median(officers_descriptive$year)
sd(officers_descriptive$year)
min(officers_descriptive$year)
max(officers_descriptive$year)

#  Unique relationships by year
avg_relationships <- officers_descriptive %>% 
  group_by(node_id, year) %>% 
  tally()

mean(avg_relationships$n)
median(avg_relationships$n)
sd(avg_relationships$n)
min(avg_relationships$n)
max(avg_relationships$n)

# Unique Entity Connections
ent_connections <- officers_descriptive %>% 
  distinct(node_id, linked_to) %>% 
  group_by(node_id) %>% 
  tally()


mean(ent_connections$n)
median(ent_connections$n)
sd(ent_connections$n)
min(ent_connections$n)
max(ent_connections$n)

