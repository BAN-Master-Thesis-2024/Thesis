# NHH - Master Thesis # 

# Library
library(ggmap)
library(tidyverse)

# Loading data
nodes_addresses <- read.csv("ICIJ/Data/nodes-addresses.csv")
relationships <- read.csv("ICIJ/Data/relationships.csv")
nodes_entities <- read.csv("ICIJ/Data/nodes-entities.csv")

# Officer matches
match_officers <- read.csv("CSV-files/Matches/match_officers_cl.csv")

# ------------------------ # 
# ------ Geocoding  ------ # 
# ------------------------ # 

# -------- # 
# Entities # 
# -------- # 

# Companies 
companies_cl <- read.csv("CSV-files/Maps_Geocoding/companies_cl.csv")

# Loop 1: for extracting registered address or registered office for entities NAs

cl <- makeCluster(6)

for (i in 1:nrow(companies_cl)) {
  
  if (!is.na(companies_cl$address[i])) {
    next
  }
  
  # Add address for NAs
  if (is.na(companies_cl$address[i])) {
    
    # Find address-relationship and corresponding primary address
    address_rel <- relationships %>% 
      filter(node_id_start == companies_cl$node_id[i]) %>% 
      filter(link %in% c("registered address", "registered office"))
    
    address_i <- nodes_addresses %>% 
      filter(node_id ==  address_rel$node_id_end[1])
    
    
    if (str_length(address_i$name[1]) > str_length(address_i$address[1]) | is.na(address_i$address[1]) ) {
      
      companies_cl$address[i] <- address_i$name[1]
      
    } 
    
    if (str_length(address_i$name[1]) < str_length(address_i$address[1]) | is.na(address_i$name[1])) {
      
      companies_cl$address[i] <- address_i$address[1]
    }
    
  }

}

stopCluster(cl)


# Loop 2: Adding names for further NAs and missing observations 
for (i in 1:nrow(companies_cl)) {
  
  if (companies_cl$address[i] == "" |  is.na(companies_cl$address[i])) {
    
    # Find address-relationship and corresponding primary address
    address_rel <- relationships %>% 
      filter(node_id_start == companies_cl$node_id[i]) %>% 
      filter(link %in% c("registered address", "registered office", "business address"))
    
    address_i <- nodes_addresses %>% 
      filter(node_id ==  address_rel$node_id_end[1])
    
    if (str_length(address_i$name[1]) > str_length(address_i$address[1]) | is.na(address_i$address[1]) ) {
      
      companies_cl$address[i] <- address_i$name[1]
      
    } 
    
    else {
      
      companies_cl$address[i] <- address_i$address[1]
      
      
    }
    
  }
  
  
}

# 490 companies miss addresses , 5190 contain addresses 

# -------- # 
# Officers # 
# -------- # 

match_officers$address <- NA

for (i in 1:nrow(match_officers)) {
  
  # Extract address-relations
  address_rel <- relationships %>% 
    filter(node_id_start == match_officers$node_id[i]) %>% 
    filter(rel_type == "registered_address")
  
  if (nrow(address_rel) < 1) {
    next
  }
  
  # Extract address
  address_i <- nodes_addresses %>% 
    filter(node_id == address_rel$node_id_end[1])
  
  if (str_length(address_i$address[1]) > str_length(address_i$name[1]) | is.na(address_i$name[1]) | address_i$name[1] == "")
  
  # Add address 
  match_officers$address[i] <- address_i$address[1]
  
  else {
    
    match_officers$address[i] <- address_i$name[1]
    
  }
  
}

# 2323 officers are connected to addresses 

# --------------------- # 
# Prepare for Geocoding # 
# --------------------- # 

entities_adr <- companies_cl %>% 
  select(node_id, name, address) %>% 
  mutate(origin = "Entity")

officers_adr <- match_officers %>% 
  select(node_id, name, address) %>% 
  mutate(origin = "Officer")

address_cl <- rbind(entities_adr, officers_adr) # Merge 

# Filter unique addresses to avoid duplicate geocoding
address_geo <- address_cl %>% 
  select(address) %>% 
  distinct(address)

address_geo <- subset(address_geo, !grepl("^B & O BERMUDEZ & OCAMPO", address)) # Remove addresses causing API to stop 
address_geo <- subset(address_geo, !grepl("^SUAREZ & ASOCIADO", address)) # Same here

address_geo <- address_geo %>% 
  filter(!row_number() %in% c(42))

colnames(address_geo) <- "address_original"

# Geocoding 
API_key <- " insert API " # This needs to be your own unique API-key
register_google(key = API_key)

# Geocoding 
cl <- makeCluster(6) # Using multiple cores 
tic() # Track time 

locations_geo <- mutate_geocode(address_geo, address_original, output = "more") # Forward geocoding

toc() # Track time 
stopCluster(cl) # Stop cluster 

write.csv(locations_geo, "CSV-files/Maps_Geocoding/Locations_Google_cl.csv")

# ------------------------------- # 
# Add coordinates and type to dfs # 
# ------------------------------- # 

# Entities 
for (i in 1:nrow(companies_cl))  {
  
  # Obtain address
  address <- locations_geo %>% 
    filter(address_original == companies_cl$address[i])
  
  if (nrow(address > 0)) {
    
    # Add lat, lon, type and loctype
    companies_cl$lat[i] <- address$lat[1]
    companies_cl$lon[i] <- address$lon[1]
    companies_cl$type_adr[i] <- address$type[1]
    companies_cl$loctype[i] <- address$loctype[1]
    
  }
  

}

# ------------------------------- # 
# Add coordinates manually   # 
# ------------------------------- # 

# Adding coordinates for unsucccesful API calls.


# Data with top addresses missing lat/lon
missing_coord <- companies_cl %>% 
  filter(is.na(lon)) %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(-n) %>% 
  mutate(lat = NA,
         lon = NA)

# Add coordinates manually for top 3 missing address-coords
# Address 1 
missing_coord$lat[1] <- "25.077721836382842"
missing_coord$lon[1] <- "-77.33962321930619"

# Address 2 
missing_coord$lat[2] <- "8.977116872644933"
missing_coord$lon[2] <- "-79.52233060467746"

# Address 3 
missing_coord$lat[3] <- "8.9924971533518"
missing_coord$lon[3] <- "-79.50688537319485"


# Loop for adding coords
for (i in 1:nrow(companies_cl)) {
  
  # Skip NAs
  if(is.na(companies_cl$address[i])) {
    
    next
  }
  
  # Address 1:
  if(companies_cl$address[i] == missing_coord$address[1]) {
    
    # Add data
    companies_cl$lat[i] <- missing_coord$lat[1]
    companies_cl$lon[i] <- missing_coord$lon[1]
    companies_cl$loctype[i] <- "approximate_manual"
    companies_cl$type_adr[i] <- "street_address_manual"
    
  }
  
  # Address 2:
  if(companies_cl$address[i] == missing_coord$address[2]) {
    
    # Add data
    companies_cl$lat[i] <- missing_coord$lat[2]
    companies_cl$lon[i] <- missing_coord$lon[2]
    companies_cl$loctype[i] <- "approximate_manual"
    companies_cl$type_adr[i] <- "street_address_manual"
    
  }
  
  # Address 2:
  if(companies_cl$address[i] == missing_coord$address[3]) {
    
    # Add data
    companies_cl$lat[i] <- missing_coord$lat[3]
    companies_cl$lon[i] <- missing_coord$lon[3]
    companies_cl$loctype[i] <- "approximate_manual"
    companies_cl$type_adr[i] <- "street_address_manual"
    
  }
  
  else {
    next
  }
  

}



write.csv(companies_cl, "CSV-files/Maps_Geocoding/companies_cl2.csv")


# Officers
for (i in 1:nrow(match_officers))  {
  
  if (is.na(match_officers$address[i])) {
    
    match_officers$lat[i] <- NA
    match_officers$lon[i] <- NA
    match_officers$type[i] <- NA
    match_officers$loctype[i] <- NA
    
    next
  }
  
  # Obtain address
  address <- locations_geo %>% 
    filter(address_original == match_officers$address[i])
  
  if (nrow(address > 0)) {
    
    # Add lat, lon, type and loctype
    match_officers$lat[i] <- address$lat[1]
    match_officers$lon[i] <- address$lon[1]
    match_officers$type[i] <- address$type[1]
    match_officers$loctype[i] <- address$loctype[1]
    
  }
  
}

write.csv(match_officers, "CSV-files/Matches/match_officers_cl2.csv")
