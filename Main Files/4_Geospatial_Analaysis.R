# NHH - Master Thesis # 

# 4 - Mapping 

# Loading packages 
library(devtools)
library(tidyverse)
library(maps)
library(ggmap)
library(ggthemes)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(OpenStreetMap)
library(osmdata)
library(parallel)

library(leaflet)
library(leaflet.providers)
library(leaflet.extras)

library(geodata)

# library(raster) 

library(gridExtra)
library(grid)
library(sf)
library(viridis)
library(countrycode)
library(geonames)
library(jsonlite)
library(tictoc)

library(huxtable)
library(ggimage)

library(gt)
library(gtExtras)

library(igraph)

library(emojifont)

# Loading data
locations_Goog <- read.csv("CSV-files/Maps_Geocoding/locations_google2.csv")  # Google API coordinates

#locations_Goog <- read.csv("CSV-files/Maps_Geocoding/Locations_Google_cl.csv")

entities <- read.csv("ICIJ/Data/nodes-entities.csv")
officers <- read.csv("ICIJ/Data/nodes-officers.csv")
relationships <- read.csv("ICIJ/Data/relationships.csv")

# Loading coordinates-dfs
match_officers <- read.csv("CSV-files/Matches/match_officers_cl2.csv")
companies_cl <- read.csv("CSV-files/Maps_Geocoding/companies_cl2.csv")

# Data Wrangling - Add year of incorporation /start of relation
locations_Goog$origin_type <- NA
locations_Goog$origin_name <- NA
locations_Goog$start_date <- NA
locations_Goog$end_date <- NA

# Loop 1 - Extract data for entities 
for (i in 1:nrow(locations_Goog)) {
  
  # Filter for entity 
  entity <- entities %>% 
    filter(node_id == locations_Goog$node_id[i])
  
  # Skip iterations for non-entities
  if (nrow(entity) == 0) {
    next
  }
  
  if (nrow(entity) > 0) {
    # Add rows
    locations_Goog$origin_type[i] <- "entity"
    locations_Goog$origin_name[i] <- entity$name
    locations_Goog$start_date[i] <- entity$incorporation_date
    locations_Goog$end_date[i] <- entity$inactivation_date
    
  }
}

# ----------------------------- # 
# Tables - Quality of locations # 
# ----------------------------- # 

# Extract locations type and subtypes 
ent_adr <- companies_cl %>% 
  select(type_adr, loctype)

colnames(ent_adr) <- c("type", "loctype")

off_adr <- match_officers %>% 
  select(type, loctype)

locations <- rbind(ent_adr, off_adr) %>%  # Bind 
  filter(!is.na(type))

# Location-type
quality_loc <- locations %>% 
  group_by(loctype) %>% 
  tally() %>% 
  arrange(-n)

quality_loc$loctype[is.na(quality_loc$loctype)] <- "missing"

# Coordinate-type 
quality_loc2 <- locations %>% 
  group_by(type) %>% 
  tally() %>% 
  arrange(-n)

quality_loc2$type[is.na(quality_loc2$type)] <- "missing"

colnames(quality_loc) <- c("Match Level", "N")
colnames(quality_loc2) <- c("Location Type", "N")

# Table 1 - Match Level, Coordinates
gt_table <- quality_loc %>% 
  gt() %>% 
  tab_header(title = "MATCH LEVEL") %>% 
  opt_all_caps() %>% 
  opt_align_table_header("left") %>% 
  tab_options(source_notes.font.size = 12,
              table.font.size = 16,
              heading.title.font.size = 24,
              table.border.top.color = "transparent",
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7",
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% 
  data_color(columns = c(N), # Fill color by value
             fn = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL
             ))

# Table 2 - Location Type 
gt_table2 <- quality_loc2 [1:10, ] %>% 
  gt() %>% 
  tab_header(title = "LOCATION TYPE") %>% 
  opt_all_caps() %>% 
  opt_align_table_header("left") %>% 
  tab_options(source_notes.font.size = 12,
              table.font.size = 16,
              heading.title.font.size = 24,
              table.border.top.color = "transparent",
              table.border.top.width = px(3),
              data_row.padding = px(7),
              table_body.hlines.color = "#f6f7f7",
              row.striping.background_color = "#fafafa") %>% 
  opt_row_striping() %>% 
  data_color(columns = c(N), # Fill color by value
             fn = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL
             ))

# Save
gt_two_column_layout(list(gt_table, gt_table2))

gtsave(gt_table, "Figures/Match_Level.html")
gtsave(gt_table2, "Figures/LocationTypel.html")

## ---------------------------------- ## 
### 4) Mapping coordinates of matches ##
## ---------------------------------- ## 

# Background for maps 
backg <- htmltools::tags$style(".leaflet-container { background: white; }" )

# ------------------------------ # 
# World Map and Colombian Map    # 
# ------------------------------ # 

# ---------------------- # 
## Extracting polygons ## 
# ---------------------- # 

# Polygons for Colombia, Colombian states and world 
Colombia <- ne_countries(country = "colombia", scale = "medium", returnclass = "sf" ) # Colombia Polygon
colombia <- st_read("JSON/colombia-outline_159.geojson") # Source : https://cartographyvectors.com/map/159-colombia-outline

bbox <- st_bbox(colombia) # Box of Colombia 

states <- ne_states(country = "colombia") # Colombian states polygon 

world2 <- st_read("JSON/countries.geojson") # World outline 

world2 <- world2[-12, ]

# ----------------------------- # 
# Sum of coordinates pr country #
# By officers and entities # 
# ----------------------------- # 

# Extract relevant rows for entities 
entities_adr <- companies_cl %>% 
  dplyr::select(node_id, name, jurisdiction, lat, lon, country_codes, loctype, type_adr) %>% 
  mutate(country = NA) %>% 
  filter(!is.na(lat))

# Extract relevant rows for officers
officers_adr <- match_officers %>% 
  dplyr::select(node_id, name, lat, lon, country_codes, loctype, type) %>% 
  mutate(country = NA)

# ----- # 
# Loop 1 # 
# ----- # 

cl <- makeCluster(6)

entities_adr$country <- NA
entities_adr$country_codes_adr <- NA

# Loop for assigning country
for (i in 1:nrow(entities_adr)) {
  
  # Assign value if country-code is NA and if coordinates are available
  if (is.na(entities_adr$country_codes[i]) | entities_adr$country_codes[i] == "" & !is.na(entities_adr$lon[i])) {
    
    # Retrieve country
    country <- maps::map.where(database = "world",
                         entities_adr$lon[i], entities_adr$lat[i])
  
    # Save in data
    entities_adr$country[i] <- country
    entities_adr$country_codes[i] <- countrycode(country, origin = "country.name", destination = "iso3c")
    
  }
  
}

stopCluster(cl)

# ----- # 
# Loop 2 # 
# ----- # 
cl <- makeCluster(6)

for (i in 1:nrow(officers_adr)) {
  
  # NA for observations with no coordinates
  if (is.na(officers_adr$lon[i])) {
    
    officers_adr$country_codes[i] <- NA
    
    next
    
  }
  
  # Assign value if coordinates are available and country-code is not assigned
  if (!is.na(officers_adr$lon[i]) & !is.na(officers_adr$country_codes[i])) {
    
    # Retrieve country
    country <- maps::map.where(database = "world",
                         officers_adr$lon[i], officers_adr$lat[i])
    
    
    # Save in data
    officers_adr$country[i] <- country
    officers_adr$country_codes[i] <- countrycode(country, origin = "country.name", destination = "iso3c")
    
  }
}

stopCluster(cl)

# Filter data
officers_adr_cl <- officers_adr %>% 
  filter(!is.na(lon))

# Summarizing nr of coordinates pr country 
# For entities 
nr_coordinates <- entities_adr %>% 
  filter(!country_codes == "" | !is.na(country_codes)) %>% # Filter NAs
  group_by(country_codes) %>% 
  tally() 

# For officers ()
nr_coordinates <- officers_adr %>% 
  filter(!is.na(country_codes)) %>% 
  filter(!country_codes == "") %>% 
  group_by(country_codes) %>% 
  tally()

# Dataframe for all
entities_adr_cl <- entities_adr %>% 
  filter(!country_codes == "" | !is.na(country_codes)) %>% 
  select(-jurisdiction) %>% 
  select(-country_codes_adr) %>% 
  rename(type = type_adr)

officers_adr_cl <- officers_adr %>% 
  filter(!is.na(country_codes))

all_adr <- rbind(entities_adr_cl, officers_adr_cl)

# For all
nr_coordinates <- all_adr %>% 
  group_by(country_codes) %>% 
  tally()

# ------------- # 
# Table Output  #
# ------------- #

# Save in separate 
coordinates_sum <- nr_coordinates %>% 
  arrange(-n)

colnames(coordinates_sum) <- c("Countrycode", "N") # Colnames
coordinates_sum$Countrycode[is.na(coordinates_sum$Countrycode)] <- "Not Assigned" # NAs

# Prepare for Figure 
coordinates_sum$iso2 <- countrycode(coordinates_sum$Countrycode, origin = "iso3c", destination = "iso2c") # ISO2 for country-flags 
coordinates_sum$Country <- countrycode(coordinates_sum$Countrycode, origin = "iso3c", destination = "country.name") # Name for displaying in figure
coordinates_sum$Continent <- countrycode(coordinates_sum$iso2, "iso2c", "continent") # Continent (for displaying in figure)

# Label
coordinates_sum$proportion <- round(coordinates_sum$N/sum(coordinates_sum$N), 2)
coordinates_sum$label <- paste0(coordinates_sum$proportion * 100," ", "%")

# Plot
country_bar <- coordinates_sum[c(1:10), ] %>% 
  ggplot(aes(x = reorder(Country, N), y = N, fill = Continent)) + 
  geom_flag(y = -100, aes(image = iso2), size = 0.1) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = N), hjust = 1.25, size = 4, color = "white") +
  geom_text(aes(label = label), hjust = -0.10, size = 7, fontface = "bold", color = "grey40") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

country_bar + 
  coord_flip()+
  expand_limits(y = -100)

grid.arrange(country1, country2, ncol = 2)

# ----- # 
# Loop # 
# ----- # 

# Loop that adds nr of coordinates pr country to world-df
for (i in 1:nrow(world2)) {
  
  # Country-name
  country <- world2$ISO_A3[i]
  
  # Coordinates found for this country 
  coords <- nr_coordinates %>% 
    filter(country_codes == country)
  
  coords_n <- coords$n[1] # Nr of coordinates
  
  # Add 0 if coords_n is NA
  if (is.na(coords_n)) {
    world2$nr_coordinates[i] <- 0
    next
  }
  
  # Add coords_n otherwise
  else {
    world2$nr_coordinates[i] <- coords_n
  }
}

world2$nr_coordinates <- as.numeric(world2$nr_coordinates) # To numeric 

# Color palettes 
# NB: Change colors so it is more visible 
world2$color[world2$nr_coordinates >= 300] <- "#00441B"
world2$color[world2$nr_coordinates >= 200 & world2$nr_coordinates < 300] <- "#116D2E"
world2$color[world2$nr_coordinates >= 100 & world2$nr_coordinates < 200] <- "#229941"
world2$color[world2$nr_coordinates >= 50 & world2$nr_coordinates < 100] <- "#33A754"
world2$color[world2$nr_coordinates >= 20 & world2$nr_coordinates < 50] <- "#66C06E"
world2$color[world2$nr_coordinates >= 10 & world2$nr_coordinates < 20] <- "#99D787"
world2$color[world2$nr_coordinates > 2 & world2$nr_coordinates < 10] <- "#CCE0A0"
world2$color[world2$nr_coordinates <= 2 & world2$nr_coordinates > 0] <- "#FFFFCC"
world2$color[world2$nr_coordinates == 0] <- "white"

# Custom Icon 
house_icon <- makeIcon(
  iconUrl = "Icon/house-solid.svg", # Free icon from fontawesome
  iconWidth = 10,
  iconHeight = 10
)

# Count occurrences within each country-polygon
# Create points in sf-format
locations <- tibble(lon = locations_Goog$lon,
                    lat = locations_Goog$lat) %>% 
  filter(!is.na(lon))

locations_Goog_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)

# New data
# Extract coordinates 
coordinates_ent <- data.frame(lon = companies_cl$lon,
                              lat = companies_cl$lat) %>%
  filter(!is.na(lon))

coordinates_off <- data.frame(lon = match_officers$lon,
                              lat = match_officers$lat) %>%
  filter(!is.na(lon))

coordinates_all <- rbind(coordinates_ent, coordinates_off)

# Locations
locations <- tibble(lon = coordinates_all$lon,
                    lat = coordinates_all$lat)

locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
coordinates_sf <- st_as_sf(coordinates_all, coords = c("lon", "lat"), crs = 4326)

# ---------------------- # 
# Plotting World Map 
# ---------------------- # 

# World layers
world_layers <- base64enc::dataURI(file = "Layers/Wrld_Layers.png")
world_layers <- paste0("<img src='", world_layers, "' style='max-width: 400px; max-height: 400px;'>")

# Color palette
values <- c(0, 1, 3, 10, 20, 50, 100, 200, 300, Inf)

custom_palette <- colorBin(
  palette = c("white","lightgreen", "green"), 
  domain = world2$nr_coordinates,
  bins = values)

test <- locations %>% 
  filter(!is.na(lat))


# Map
world_map <- leaflet(options = leafletOptions(zoomControl = FALSE, backgroundColor = "#FFFFFF")) %>%
  addProviderTiles(providers$CartoDB) %>% 
  addPolygons(data = world2, color = "grey", fillColor = ~color, weight = 2.5, fillOpacity = 0.4,
              label = ~paste(ADMIN, ": ", nr_coordinates)) %>% # Add polygons of world-map 
  addMarkers(lng = all_adr$lon, lat = all_adr$lat,
             clusterOptions = markerClusterOptions(maxClusterRadius = 100),
             icon = house_icon) %>% 
  #addMarkers(lng = companies_cl$lon, lat = companies_cl$lat,
       #      clusterOptions = markerClusterOptions(maxClusterRadius = 100),
        #     icon = house_icon) %>% 
 # addMarkers(lng = match_officers$lon, lat = match_officers$lat,
     #        clusterOptions = markerClusterOptions(maxClusterRadius = 100),
      #       icon = house_icon) %>%  # Add markers, use custom icon 
  addHeatmap(data = coordinates_ent, radius = 8, blur = 20) %>% 
  addHeatmap(data = coordinates_off, radius = 8, blur = 20) %>% 
  htmlwidgets::onRender("
     function(el, x) {
         el.style.backgroundColor = '#FFFFFF';
     }
  ") %>% 
  clearTiles() %>% 
  addProviderTiles(providers$CartoDB.VoyagerOnlyLabels)

leaflet(options = leafletOptions(zoomControl = FALSE, backgroundColor = "#FFFFFF")) %>%
  addProviderTiles(providers$CartoDB) %>% 
  addPolygons(data = world2, color = "grey", fillColor = ~color, weight = 2.5, fillOpacity = 0.4,
              label = ~paste(ADMIN, ": ", nr_coordinates)) %>% # Add polygons of world-map 
  addMarkers(lng = locations$lon, lat = locations$lat,
             clusterOptions = markerClusterOptions(maxClusterRadius = 100),
             icon = house_icon)

# ---------------------- # 
# Plotting Caribbean map 
# ---------------------- # 

# Extract Caribbean countries (NB: Only Caribbean or surrounding countries as well?)
countries <- ne_countries(scale = "medium", returnclass = "sf")
caribbean <- countries[countries$subregion == "Caribbean", ]
caribbean <- countries[countries$region_wb %in% c( "Latin America & Caribbean", "North America"),]

caribbean <- caribbean %>% 
  filter(!name %in% c("Argentina", "Chile", "Brazil", "Uruguay", "Paraguay", "Peru", "Bolivia", "Guyana", "Suriname", "Canada"))

florida <- ne_states(country = "united states of america") %>% 
  filter(name == "Florida") # Florida due to the near proximity

caribbean$geometry[36] <- st_geometry(florida$geometry)

caribbean <- st_transform(caribbean, crs = "+proj=longlat +datum=WGS84") # 

# Count coordinates within each country
caribbean_sf <- st_as_sf(caribbean$geometry)

caribbean_summary <- caribbean_sf %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

# Extract nr of coordinates by assigned country
nr_coords <- world2 %>% 
  filter(ISO_A3 %in% c(caribbean$su_a3))

caribbean$nr_coords <- as.numeric(nr_coords$nr_coordinates) # Save 

# Centroids for plotting name of countries (filter for countries with > 0 coords)
caribbean$centroid <- st_centroid(caribbean$geometry)
centroids <- st_coordinates(caribbean$centroid)
caribbean$centroid_lon <- centroids[, "X"]
caribbean$centroid_lat <- centroids[, "Y"]

# Filter
centroids <- caribbean %>% 
  filter(nr_coords > 10)

centroids$name[14] <- "Florida (U.S.)"

# Color palette
values <- c(0, 50, 100, 300, 400, 500, 600, 800, Inf)

custom_palette <- colorBin(
  palette = c("yellow", "orange", "red"), 
  domain = caribbean$nr_coords,
  bins = values)

# flags
centroids$flag <- c("🇦🇼", "🇧🇸", "🇧🇲", "🇧🇧", "🇨🇴", "🇨🇷", "🇰🇾", "🇩🇴", "🇪🇨", "🇬🇹", "🇲🇽", "🇵🇦", "🇸🇻", 
                    "🇺🇸", "🇻🇪", "🇻🇬")

# label
centroids$label <- paste(centroids$name, centroids$flag)

# Gradients
# Map
leaflet(data = caribbean, options = leafletOptions(zoomControl = FALSE, backgroundColor = "#FFFFFF")) %>%
  addTiles() %>% 
  addPolygons(data = caribbean, color = "grey", weight = 1.5,
              fillColor = ~custom_palette(nr_coords),
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5, bringToFront = TRUE)) %>%
#  addMarkers(lng = all_adr$lon, lat = all_adr$lat,
       #      clusterOptions = markerClusterOptions(),
       #      icon = house_icon) %>% 
  addMarkers(lng = coordinates_all$lon, lat = coordinates_all$lat,
             clusterOptions = markerClusterOptions(),
             icon = house_icon) %>% 
  addHeatmap(data = coordinates_ent, minOpacity = 75, radius = 10, blur = 25) %>% 
  addHeatmap(data = coordinates_off, minOpacity = 75, radius = 10, blur = 25) %>% 
  addLabelOnlyMarkers(data = centroids,
                      ~centroid_lon, ~centroid_lat, label =  ~as.character(label), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  size = 10,
                                                  style = list("font-weight" = "bold",
                                                               "font-size" = "13px"))) %>%
  clearTiles() %>% 
  htmlwidgets::onRender("
      function(el, x) {
          el.style.backgroundColor = '#FFFFFF';
      }
  ")

# ----------------- # 
# Colombia Addresses # 
# ----------------- # 
colombia_sf <- st_as_sf(colombia$geometry)

colombia_points <- st_intersection(locations_sf, colombia_sf)

# Filter addresses for Colombia 
col_locations <- all_adr %>% 
  filter(country_codes == "COL")

# Map-layers
col_layers <- base64enc::dataURI(file = "Layers/Colombia_Layers.png")
col_layers <- paste0("<img src='", col_layers, "' style='max-width: 250px; max-height: 250px;'>")

# Flag
flag_col <- readLines("Flags/co.svg")
flag_col <- gsub('<svg', '<svg width="200" height="200"', flag_col)

# Count coordinates by state
# Format polygons as sf-element
states_st <- st_as_sf(states$geometry)

# Count within
sf_summary <- states_st %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

states$within <- sf_summary$counts # Add to polygon df

# Color palette
# Coloring
labels <- c("0-10", "11-20", "20-50", "50-100", "101-200", "200-300", "300+")
values <- c(0, 10, 20, 50, 100, 200, 300, 500)

custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = states$within,
  bins = values)

# Colombia_Map
Colombia_Map <- leaflet(data = states, options = leafletOptions(zoomControl = FALSE)) %>% # Add tiles 
  addTiles(options = tileOptions(background_color = "#FFFFFF")) %>% 
 # addRectangles(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90, 
      #          fillColor = "#FFFFFF", fillOpacity = 1, weight = 0) %>% 
  addPolygons(data = states,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(~longitude, ~latitude, label =  ~as.character(name), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list(fontWeight = "bold"))) %>%  # Add labels
  addMarkers(lng = coordinates_all$lon, lat = coordinates_all$lat, 
             clusterOptions = markerClusterOptions(),
             icon = house_icon) %>% 
#  addMarkers(lng = col_locations$lon, lat = col_locations$lat,
      #       clusterOptions = markerClusterOptions(),
       #      icon = house_icon) %>% 
  clearTiles() %>% # Remove tiles 
#  addLegend(pal = custom_palette,
      #      values = values,
       #     labels = labels,
        #    position = "bottomright",
       #     title = NULL) %>% 
  addControl(html = col_layers, position = "bottomright") %>% 
  addControl(html = flag_col, position = "topright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::onRender("
      function(el, x) {
          el.style.backgroundColor = '#FFFFFF';
      }
  ")

# ----------------------- # 
# Medellin and Bogota maps 
# ----------------------- # 

# Polygons for districts/communes in Bogota and Medellin
bogota_pol <- st_read("JSON/poligonos-localidades.geojson") # Retrieved from "mapas.bogota.gov.co"
medellin_pol <- st_read("JSON/medellin.geojson") # Retrieved from davixcky/medellin.geojson (github)

# --------- #
# Cleaning #
# --------- #
bogota_pol <- bogota_pol[-9, ] # Removing Sumapaz
medellin_pol <- medellin_pol[c(1:15, 23), ] # Selecting districts (communes) only

# Calculating centroids for commune-polygons
bogota_pol$centroid <- st_centroid(bogota_pol$geometry)
medellin_pol$centroid <- st_centroid(medellin_pol$geometry)

# Extracting lat and lon for plotting labels 
bogota_pol$lat <- st_coordinates(bogota_pol$centroid)[, "Y"]
bogota_pol$lon <- st_coordinates(bogota_pol$centroid)[, "X"]

medellin_pol$lat <- st_coordinates(medellin_pol$centroid)[, "Y"]
medellin_pol$lon <- st_coordinates(medellin_pol$centroid)[, "X"]

# Transform names of states
bogota_pol$Nombre.de.la.localidad <- str_to_title(bogota_pol$Nombre.de.la.localidad)

# Layers
bog_layers <- base64enc::dataURI(file = "Layers/Bogota_Layers.png")
bog_layers <- paste0("<img src='", bog_layers, "' style='max-width: 350px; max-height: 350px;'>")

med_layers <- base64enc::dataURI(file = "Layers/Medellin_Lay.png")
med_layers <- paste0("<img src='", med_layers, "' style='max-width: 350px; max-height: 350px;'>")

# ----------- #
# Bogota Map #
# ---------- #

# Count number of locations within each polygon

# Format polygons as sf-element
bogota_st <- st_as_sf(bogota_pol$geometry)

# Count within
data_sf_summary <- bogota_st %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

bogota_pol$within <- data_sf_summary$counts # Add to polygon df

# Coloring
labels <- c("0", "1-5", "6-10", "11-20", "21-40", "41-60", "61-80", "81-100", "101+")
values <- c(0, 5, 10, 20, 40, 60, 80, 100, 200, 400, Inf)

custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = bogota_pol$within,
  bins = values)

# Map of addresses in Bogota 
bogota_map <- leaflet(data = bogota_pol) %>%
 # addRectangles(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90, 
  #              fillColor = "#FFFFFF", fillOpacity = 1, weight = 0) %>% # White background
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%  # Fit Bounds 
  setView(lng = -74.063644, lat = 4.624335, zoom = 11) %>% # Set view to Bogota
  addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%  # Add map-tiles 
  addPolygons(data = bogota_pol,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(~lon, ~lat, label =  ~as.character(Nombre.de.la.localidad), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list(fontWeight = "bold"))) %>% 
  addMarkers(lng = coordinates_all$lon, lat = coordinates_all$lat, 
             clusterOptions = markerClusterOptions(),
             icon = house_icon) %>% 
  addHeatmap(data = coordinates_all, minOpacity = 75, radius = 10, blur = 25) %>% 
  clearTiles() %>% 
  #addLegend(pal = custom_palette,
    #        values = values,
       #     labels = labels,
       #     position = "bottomright",
       #     title = NULL) %>% 
  addControl(html = bog_layers, position = "bottomright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::onRender("
      function(el, x) {
          el.style.backgroundColor = '#FFFFFF';
      }
  ")
  
# ----------- #
# Medellin Map #
# ---------- #

# Format polygons as sf-element
medellin_st <- st_as_sf(medellin_pol$geometry)

# Count within
data_sf_summary2 <- medellin_st %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

medellin_pol$within <- data_sf_summary2$counts # Add to polygon df

# Coloring
labels <- c("0", "1-5", "6-10", "11-20", "21-40", "41-60", "61-80", "81-100", "101+")
values <- c(0, 5, 10, 20, 40, 60, 80, 100, 200, Inf)

custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = bogota_pol$within,
  bins = values)

medellin_points <- st_filter(points_sf, medellin_pol)

test <- st_filter(points_sf, colombia)

medellin_points$coordinates <- st_coordinates(medellin_points)

medellin_points$lon <- medellin_points$coordinates[, "X"]
medellin_points$lat <- medellin_points$coordinates[, "Y"]


# Map of addresses in Medellin
medellin_map <- leaflet() %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%  # Fit Bounds 
  setView(lng = -75.590553, lat = 6.230833, zoom = 13) %>% # Set view to Medellin
  addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%  # Add map-tiles 
  addPolygons(data = medellin_pol,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = medellin_pol, ~lon, ~lat, label =  ~as.character(NOMBRE), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list(fontWeight = "bold"))) %>% 
  addMarkers(lng = medellin_points$lon, lat = medellin_points$lat,
             clusterOptions = markerClusterOptions(),
             icon = house_icon) %>% # Add markers, use custom icon 
  addHeatmap(data = medellin_points, minOpacity = 75, radius = 10, blur = 25) %>% 
  clearTiles() %>% # Remove tiles
#  addLegend(pal = custom_palette,
       #     values = values,
       #     labels = labels,
        #    position = "bottomright",
        #    title = NULL) %>% 
  addControl(html = med_layers, position = "bottomright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::onRender("
      function(el, x) {
          el.style.backgroundColor = '#FFFFFF';
      }
  ")

# --------------------------------------- # 
# Heat Maps for Selected Tax Havens  # 
# --------------------------------------- # 

# ----------------- # 
# Data Wrangling
# ---------------- # 

# ------ # 
# Polygons for different regions
# ------ # 

# Bahamas 
bahamas <- ne_states(country = "the bahamas")

florida <- ne_states(country = "united states of america") %>% 
  filter(name == "Florida") # Florida due to the near proximity

bahamas <- rbind(bahamas, florida)

# Bermuda
bermuda <- st_read("JSON/bermuda.geojson")
bermuda <- st_zm(bermuda) # Transform from Multipolygon Z to Multipolygon

# Panama
panama <- ne_states(country = "panama")

# Malta 
malta <- st_read("JSON/malta.geojson") #  free for use: cite source “Research Innovation Unit https://riu.gov.mt
malta <- st_zm(malta) # Transform from Multipolygon Z to Multipolygon

# Aruba
aruba <- st_read("JSON/Aruba_regions.geojson") # Source: arcgis (Social Atlas 2020_Regio)

# Bahamas (TM-BRB_Administrative Districts - 2000, caribbeanmarineatlas.net) 
barbados <- st_read("JSON/barbados.geojson")
barbados <- st_transform(barbados, "+proj=longlat +datum=WGS84")

# British Virgin Islands 
vgb <- st_read("JSON/vgb.geojson")

# Flags for all regions 
flag_bah <- readLines("Flags/Bahamas_flag.svg")
flag_bah <- gsub('<svg', '<svg width="200" height="200"', flag_bah)

flag_ber <- readLines("Flags/Bermuda_flag.svg")
flag_ber <- gsub('<svg', '<svg width="200" height="200"', flag_ber)

flag_bar <- readLines("Flags/Barbados_flag.svg")
flag_bar <- gsub('<svg', '<svg width="200" height="200"', flag_bar)

flag_mal <- readLines("Flags/malta_flag.svg")
flag_mal <- gsub('<svg', '<svg width="200" height="200"', flag_mal)

flag_pan <- readLines("Flags/panama_flag.svg")
flag_pan <- gsub('<svg', '<svg width="200" height="200"', flag_pan)

flag_vgb <- readLines("Flags/vg.svg")
flag_vgb <- gsub('<svg', '<svg width="150" height="150"', flag_vgb)


# -------------------------- # 
# Tax Haven 1  - The Bahamas 
# -------------------------- # 
bahamas_st <- st_as_sf(bahamas$geometry)

# Preparing 
bahamas$centroid <- st_centroid(bahamas$geometry)
bahamas$lat <- st_coordinates(bahamas$centroid)[, "Y"]
bahamas$lon <- st_coordinates(bahamas$centroid)[, "X"]

# Count points within regions
bahamas_sf_summary <- st_as_sf(bahamas$geometry) %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

bahamas$within <- bahamas_sf_summary$counts # Add to polygon df

# Color palette
custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = bahamas$within,
  bins = values)

# Layers
bah_layers <- base64enc::dataURI(file = "Layers/Bah_Lay.png")
bah_layers <- paste0("<img src='", bah_layers, "' style='max-width: 700px; max-height: 700px;'>")

# Map
bahamas_map <- leaflet(coordinates_all, options = leafletOptions(zoomControl = FALSE)) %>% 
  setView(lat = 25, lng = -77, zoom = 6) %>% 
  addTiles() %>%
  addHeatmap(data = coordinates_all, radius = 10) %>% 
  addPolygons(data = bahamas,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = bahamas[c(14, 31),], ~lon, ~lat, label =  ~as.character(name), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list(fontWeight = "bold"))) %>% 
  clearTiles() %>% 
  addControl(html = flag_bah, position = "topright", className = "fieldset { border: 0;}") %>% 
  addControl(html = bah_layers, position = "bottomright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::prependContent(backg)

# -------------------------- # 
# Tax Haven 2 - Malta 
# -------------------------- # 

# Preparing
malta$centroid <- st_centroid(malta$geometry)
malta$lat <- st_coordinates(malta$centroid)[, "Y"]
malta$lon <- st_coordinates(malta$centroid)[, "X"]

# Count within
malta_sf_summary <- st_as_sf(malta$geometry) %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

malta$within <- malta_sf_summary$counts # Add to polygon df

# Color palette
custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = malta$within,
  bins = values)

# Layers
mal_layers <- base64enc::dataURI(file = "Layers/Mal_Layers.png")
mal_layers <- paste0("<img src='", mal_layers, "' style='max-width: 350px; max-height: 350px;'>")

# Map
malta_map <- leaflet(coordinates_all, options = leafletOptions(zoomControl = FALSE)) %>% 
  setView(lng = 14.5146, lat = 35.8980, zoom = 10) %>%
  addTiles() %>%
  addHeatmap(data = coordinates_all, radius = 5) %>% 
  addPolygons(data = malta,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = malta[malta$within > 9, ], ~lon, ~lat, label =  ~as.character(LAU_Latin), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list(fontWeight = "bold"))) %>% 
  clearTiles() %>% 
  addControl(html = flag_mal, position = "topright", className = "fieldset { border: 0;}") %>% 
  addControl(html = mal_layers, position = "bottomright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::prependContent(backg)


# -------------------------- # 
# Tax Haven 3 - Bermuda
# -------------------------- # 

# Preparing 
bermuda$centroid <- st_centroid(bermuda$geometry)
bermuda$lat <- st_coordinates(bermuda$centroid)[, "Y"]
bermuda$lon <- st_coordinates(bermuda$centroid)[, "X"]

# Count points within regions
bermuda_sf_summary <- st_as_sf(bermuda$geometry) %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

bermuda$within <- bermuda_sf_summary$counts # Add to polygon df

# Color palette
custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = bermuda$within,
  bins = values)

# Layers
ber_layers <- base64enc::dataURI(file = "Layers/Ber_Layers.png")
ber_layers <- paste0("<img src='", ber_layers, "' style='max-width: 650px; max-height: 650px;'>")

# Map
bermuda_map <- leaflet(coordinates_all, options = leafletOptions(zoomControl = FALSE)) %>% 
  setView(lat = 32, lng = -65, zoom = 10) %>% 
  addTiles() %>%
  addHeatmap(data = coordinates_all, radius = 10) %>% 
  addPolygons(data = bermuda,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = bermuda, ~lon, ~lat, label =  ~as.character(ParishName), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list(fontWeight = "bold"))) %>% 
  clearTiles() %>% 
  addControl(html = flag_ber, position = "topright", className = "fieldset { border: 0;}") %>% 
  addControl(html = ber_layers, position = "bottomright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::prependContent(backg)

# -------------------------- # 
# Tax Haven 4 - Panama
# -------------------------- # 
# Preparing 
panama$centroid <- st_centroid(panama$geometry)
panama$lat <- st_coordinates(panama$centroid)[, "Y"]
panama$lon <- st_coordinates(panama$centroid)[, "X"]

# Count points within regions
panama_sf_summary <- st_as_sf(panama$geometry) %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

panama$within <- panama_sf_summary$counts # Add to polygon df

# Color palette
custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = panama$within,
  bins = values)

# Layers
pan_layers <- base64enc::dataURI(file = "Layers/Pan_Layers.png")
pan_layers <- paste0("<img src='", pan_layers, "' style='max-width: 375px; max-height: 375px;'>")

# Map
panama_map <- leaflet(coordinates_all, options = leafletOptions(zoomControl = FALSE)) %>% 
  setView(lat = 9, lng = -80, zoom = 8) %>% 
  addTiles() %>%
  addHeatmap(data = coordinates_all, radius = 10) %>% 
  addPolygons(data = panama,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = panama, ~lon, ~lat, label =  ~as.character(name), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list(fontWeight = "bold"))) %>% 
  clearTiles() %>% 
  addControl(html = flag_pan, position = "topright", className = "fieldset { border: 0;}") %>% 
  addControl(html = pan_layers, position = "bottomright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::prependContent(backg)

# -------------------------- # 
# Tax Haven 5 - Barbados
# -------------------------- # 
# Preparing 

barbados$centroid <- st_centroid(barbados$geometry)
barbados$lat <- st_coordinates(barbados$centroid)[, "Y"]
barbados$lon <- st_coordinates(barbados$centroid)[, "X"]

# Count points within regions
barbados_sf_summary <- st_as_sf(barbados$geometry) %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

barbados$within <- barbados_sf_summary$counts # Add to polygon df

# Layers
# Layers 
bar_layers <- base64enc::dataURI(file = "Layers/Barbados_Layers.png")
bar_layers <- paste0("<img src='", bar_layers, "' style='max-width: 300px; max-height: 300px;'>")

# Color palette
custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = barbados$within,
  bins = 5)

# Map
barbados_map <- leaflet(coordinates_all, options = leafletOptions(zoomControl = FALSE)) %>% 
  setView(lat = 13, lng = -59, zoom = 10) %>% 
  addTiles() %>%
  addHeatmap(data = coordinates_all, radius = 5) %>% 
  addPolygons(data = barbados,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = barbados, ~lon, ~lat, label =  ~as.character(Name2), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list(fontWeight = "bold"))) %>% 
  clearTiles() %>% 
  addControl(html = flag_bar, position = "topright", className = "fieldset { border: 0;}") %>% 
  addControl(html = bar_layers, position = "bottomright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::prependContent(backg)

# -------------------------- # 
# Tax Haven 6 - Aruba
# -------------------------- # 
# Preparing 
aruba_valid <- st_make_valid(aruba$geometry)
aruba_centroid <- st_centroid(aruba_valid)

aruba$lat <- st_coordinates(aruba_centroid)[, "Y"]
aruba$lon <- st_coordinates(aruba_centroid)[, "X"]

# Count points within regions
aruba_sf_summary <- st_as_sf(aruba_valid) %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

aruba$within <- aruba_sf_summary$counts # Add to polygon df

# Color palette
custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = aruba$within,
  bins = 5)

flag_awb <- readLines("Flags/Aruba_flag.svg", warn = FALSE)
flag_awb_resize <- gsub('<svg', '<svg width="200" height="200"', flag_awb)

# Layers 
awb_layers <- base64enc::dataURI(file = "Layers/Aruba_Layers.png")
awb_layers <- paste0("<img src='", awb_layers, "' style='max-width: 450px; max-height: 450px;'>")

# Map
aruba_map <- leaflet(coordinates_all, options = leafletOptions(zoomControl = FALSE)) %>% 
  setView(lat = 12.5, lng = -70, zoom = 10) %>% 
  addTiles() %>%
  addHeatmap(data = coordinates_all, radius = 6) %>% 
  addPolygons(data = aruba,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = aruba, ~lon, ~lat, label =  ~as.character(GAC1_NAAM), 
                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>% 
  clearTiles() %>% 
  addControl(html = flag_awb_resize, position = "topright", className = "fieldset { border: 0;}") %>% 
  addControl(html = awb_layers, position = "bottomright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::prependContent(backg)


# ----------------------------------- # 
# Tax Haven 7 - British Virgin Islands
# ----------------------------------- # 
vgb_st <- st_as_sf(vgb$geometry)
vgb_sf <- st_make_valid(vgb_st)

# Preparing 
vgb$centroid <- st_centroid(vgb$geometry)
vgb$lat <- st_coordinates(vgb$centroid)[, "Y"]
vgb$lon <- st_coordinates(vgb$centroid)[, "X"]

# Count points within regions
vgb_sf_summary <- st_as_sf(vgb$geometry) %>% 
  mutate(counts = lengths(st_intersects(., locations_Goog_sf)))

vgb$within <- vgb_sf_summary$counts # Add to polygon df

# Color palette
custom_palette <- colorBin(
  palette = c("lightyellow", "yellow", "orange", "red"), 
  domain = vgb$within,
  bins = values)

# Map
vgb_map <- leaflet(coordinates) %>% 
  setView(lat = 18, lng = -64, zoom = 10) %>% 
  addTiles() %>%
  addHeatmap(data = coordinates_sf, radius = 10) %>% 
  addPolygons(data = vgb,
              fillColor = ~custom_palette(within),
              color = "grey", weight = 1.5,
              highlight = highlightOptions(weight = 2, color = ~blues9, fillOpacity = 0.5,
                                           bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = vgb, ~lon, ~lat, label =  ~as.character(ADM0_EN), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>% 
  clearTiles() %>% 
  addControl(html = flag_vgb, position = "topright", className = "fieldset { border: 0;}") %>% 
  htmlwidgets::prependContent(backg)

##### testing ######

# -------------------------- # 
# Map of Connected countries 
# -------------------------- # 
# Loading relevant data
relationships_cl <- read.csv("CSV-files/relationships_cl.csv")

match_entities <- read.csv("CSV-files/match_entities_cl.csv")
match_officers <- read.csv("CSV-files/match_officers_cl.csv")

# ICIJ-data
nodes_addresses <- read_csv("ICIJ/Data/nodes-addresses.csv") # Addresses of nodes 
nodes_entities <- read_csv("ICIJ/Data/nodes-entities.csv")   # Entity-nodes 
nodes_officers <- read_csv("ICIJ/Data/nodes-officers.csv")   # Officers-nodes

# Locations 
locations_Goog <- read.csv("CSV-files/locations_google.csv")

# Empty dataframe for storing data 
connections <- NULL

cl <- makeCluster(6) # Cl
tic() # Track time 

# Loop for extracting links between officers and entities
for (i in 1:nrow(match_officers)) {
  
  ## 1 - Extract match ## 
  # Extract match
  match <- match_officers[i, ]
  id <- match$node_id # ID of match
  
  ## 2 - Extract relationships ## 
  # Extract node_relations
  relations <- relationships_cl %>% 
    filter(node_id_start == id)
  
  # Extract company-relations
  relations_comp <- relations %>% 
    filter(rel_type %in% c("officer_of", "intermediary_of"))
  
  comp_id <- c(relations_comp$node_id_end) # ID of companies
  
  ## 3 - Find linked countries/jurisdictions ## 
  # Extract entities
  entities <- nodes_entities %>% 
    filter(node_id %in% comp_id) 
  
  # Save columns of interest 
  connections_df <- tibble(node_id = rep(id, nrow(entities)),
                           name = rep(match$name, nrow(entities)),
                           country_origin = rep(match$countries, nrow(entities)),
                           term = rep(match$term, nrow(entities)),
                           node_id_comp = entities$node_id,
                           comp_name = entities$name,
                           jurisdiction = entities$jurisdiction_description,
                           incorporation_date = entities$incorporation_date,
                           inactivation_date = entities$inactivation_date,
                           linked_country = entities$countries)
  
  connections <- rbind(connections, connections_df)

}

stopCluster(cl) # Stop cl
toc() # Track time 

write_csv(connections, "CSV-files/connections.csv") # Save for future use

connections <- read.csv("CSV-files/connections.csv")
