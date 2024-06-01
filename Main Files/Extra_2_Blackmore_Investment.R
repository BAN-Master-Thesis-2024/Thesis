# NHH - Master Thesis # 

# Extra 2 # 

# Globe with focal point on Blackmore Investment A.V.V # 

# Packages
library(maps)
library(plotrix)
library(sf)
library(tidyverse)
library(osmdata)
library(RColorBrewer)
library(giscoR)
library(png)

# ----------------------- # 
# Plotting a Globe
# ----------------------- # 

# String for polygons and oceans
crs_string <- "+proj=ortho +lon_0=-70 +lat_0=12.5"

# background for globe 
ocean <- st_point(x = c(12.5,-70)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = crs_string)

# country polygons 
world <- gisco_countries %>% 
  st_intersection(ocean %>% st_transform(4326)) %>% # select visible area only
  st_transform(crs = crs_string) # reproject to ortho

# Fill color for Colombia
world$fill_color <- ifelse(world$ISO3_CODE == "COL", "interesting", "dull")
world$fill_color2 <- ifelse(world$ISO3_CODE %in% caribbean$iso_a3, "interesting", "dull")

world$fill_color2[135] <- "dull"

# Colombia flag and polygon
col_flag <- readPNG("Flags/Col_Flag.png")
colombia <- st_read("JSON/colombia-outline_159.geojson") 

# Plot
ggplot(data = world) +
  geom_sf(data = ocean, fill = "darkblue", color = NA) + # bg
  geom_sf(aes(fill = fill_color), lwd = .1, color = "black") + # land over ocean
  scale_fill_manual(values = c("interesting" = "yellow",
                               "dull" = "darkgreen"),
                    guide = "none") +
  geom_rect(aes(xmin = -264417, xmax = 274417, ymin = -264417, ymax = 264417), color = "red", fill = NA, lwd = 0.1) +
  theme_void()

# Caribbean globe 
ggplot(data = world) +
  geom_sf(data = ocean, fill = "darkblue", color = NA) + # bg
  geom_sf(aes(fill = fill_color2), lwd = .1, color = "black") + # land over ocean
  scale_fill_manual(values = c("interesting" = "yellow",
                               "dull" = "darkgreen"),
                    guide = "none") +
  geom_sf(data = florida, fill = "yellow", color = "black") +
  theme_void()

# ------------------------------- # 
# Country polygon with flag overlay
# ------------------------------- # 
colombia <- gisco_get_countries(country = "Colombia", epsg = 3857)

# Colombia
col <- gisco_get_countries(
  country = "Colombia",
  epsg = 3857,
  resolution = 1
)

col <- st_transform(col, crs = "+proj=ortho +lon_0=-70 +lat_0=12.5")

# Flag
flag_rast <- rasterpic_img(col,
                           "Flags/Col_Flag.png",
                           mask = TRUE,
                           crop = TRUE)

# Plot 
tm_shape(flag_rast) +
  tm_rgb() + 
  tm_layout(bg.color = "white")

# ----------------------- # 
# Aruba map 
# ----------------------- # 

# Aruba_polygon regions
aruba <- st_read("JSON/Aruba_regions.geojson")

# Obtain layers for roads
osm_aruba_road <- getbb("Aruba") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

# Blackmore Investment A.V.V  - address
blackmore_inv <- tibble(lat = 12.515714892174486,
                        lon = -70.03460423545808)

# Aruba coords
aruba_sf <- st_as_sf(aruba$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

aruba_sf <- st_make_valid(aruba_sf)

awb_coords <- st_filter(points_sf, aruba_sf)

awb_coords$eters_laea <- st_transform(awb_coords, 4326)

# Plot Aruba Map
ggplot() + 
  geom_sf(data = aruba, fill = "grey95", color = "black", alpha = 0.3) + 
  geom_sf(data = osm_aruba_road$osm_lines, alpha = 0.6, color = "grey70") + 
  geom_point(data = blackmore_inv, aes(x = lon, y = lat), color = "red", size = 5, alpha = 2, shape = 19) + 
  geom_text(data = blackmore_inv, aes(x = lon , y = lat + 0.01, 
                                      label = "Blackmore Investment A.V.V"), color = "black", alpha = 2, size = 5,
            fontface = "bold") +
  geom_sf(data = awb_coords$eters_laea, size = 0.1, alpha = 1/10) +
  theme_void() + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-70.08, -69.85))

# Coords
awb_coords$coordinates <- st_coordinates(awb_coords)
awb_coords$lon <- awb_coords$coordinates[, "X"]
awb_coords$lat <- awb_coords$coordinates[, "Y"]

# Plot 
ggplot() +
  stat_density2d(data = awb_coords,
                 aes(x = lon, y = lat, fill = ..level..),
    contour_var = "ndensity",
    alpha = 1/2,
    geom = "polygon",
    h = 0.04) + 
  scale_fill_gradientn(colors = c("white", "orange")) +
  geom_sf(data = aruba, fill = "grey95", color = "black", alpha = 0.3) + 
  geom_sf(data = osm_aruba_road$osm_lines, alpha = 0.8, color = "grey70") + 
  geom_text(data = blackmore_inv, aes(x = lon , y = lat + 0.01, 
                                      label = "Blackmore Investment A.V.V"), color = "black", alpha = 2, size = 5,
            fontface = "bold") +
  geom_point(data = blackmore_inv, aes(x = lon, y = lat), color = "red", size = 5, alpha = 2, shape = 19) +
  theme_void() + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-70.08, -69.85))

# ------------------------------------ # 
# Explore Entities Registered at Address
# ----------------------------------- # 

# 88007060 - Node-id for L.G. Smith BLVD 48

# Relationships 
relationships <- read.csv("ICIJ/Data/relationships.csv")
entities <- read.csv("ICIJ/data/nodes-entities.csv")
officers <- read.csv("ICIJ/data/nodes-officers.csv")

# Entities 
rel_entities_lg_blv <- relationships %>% 
  filter(node_id_end == "88007060")

entities_lg_blv <- entities %>% 
  filter(node_id %in% c(rel_entities_lg_blv$node_id_start))

# Extract incorporation year 
entities_lg_blv$incorporation_format <- anytime(entities_lg_blv$incorporation_date)
entities_lg_blv$incorporation_year <- as.numeric(format(entities_lg_blv$incorporation_format, "%Y"))

entities_lg_blv$struck_off_format <- anytime(entities_lg_blv$struck_off_date)
entities_lg_blv$struck_off_year <- as.numeric(format(entities_lg_blv$struck_off_format, "%Y"))


# Time-series data
entities_struck <- entities_lg_blv %>% 
  group_by(struck_off_year) %>% 
  tally()

entities_incorp <- entities_lg_blv %>% 
  group_by(incorporation_year) %>% 
  tally()

ggplot(entities_lg_blv) + 
  geom_bar(aes(x = incorporation_year), fill = "#3E90DF", color = "white", alpha = 0.4) +
 # geom_text(stat = "count", aes(x = incorporation_year, label = ..count..),
      #      vjust = 1.5, size = 4, color = "white", fontface = "bold") + +
  geom_line(data = entities_struck[1:18, ], aes(y = n, x = struck_off_year), color = "#F0303E", lty = "longdash") +
  geom_ribbon(data = entities_struck[1:18, ], aes(y = n, x = struck_off_year, ymin = 0, ymax = n), fill = "#FFD201", alpha = 0.4) +
  geom_point(data = entities_struck[1:18, ], aes(y = n, x = struck_off_year), color = "#FFD201", size = 7) +
  geom_text(data = entities_struck[1:18, ], aes(y = n, x = struck_off_year, label = n,),
            color = "grey40", fontface = "bold", alpha = 2, size = 4 ) +
  ggthemes::theme_fivethirtyeight() + 
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black")) + 
  xlim(1985, 2010)

# colors
colors <- c("Incorporation" = "darkred", "Struck Off" = "#FFD201")

ggplot() + 
  
  # Incorporations
  geom_line(data = entities_incorp[10:27, ], aes(y = n , x = incorporation_year, color = "Incorporation")) + 
  geom_ribbon(data = entities_incorp[10:27, ], aes(y = n, x = incorporation_year, ymin = 0, ymax = n), fill = "darkred", alpha = 0.3) +
  geom_point(data = entities_incorp[10:27, ], aes(y = n, x = incorporation_year), color = "darkred", size = 7) +
  geom_text(data = entities_incorp[10:27, ], aes(y = n, x = incorporation_year, label = n,),
            color = "white", fontface = "bold", alpha = 2, size = 4) + 
  
  # Struck Off
  geom_line(data = entities_struck[1:18, ], aes(y = n, x = struck_off_year, color = "Struck Off")) +
  geom_ribbon(data = entities_struck[1:18, ], aes(y = n, x = struck_off_year, ymin = 0, ymax = n), fill = "#FFD201", alpha = 0.3) +
  geom_point(data = entities_struck[1:18, ], aes(y = n, x = struck_off_year), color = "#FFD201", size = 7) +
  geom_text(data = entities_struck[1:18, ], aes(y = n, x = struck_off_year, label = n,),
            color = "grey40", fontface = "bold", alpha = 2, size = 4) + 
  ggthemes::theme_fivethirtyeight() + 
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(fill = "#f5f7fd"),
        panel.background = element_rect(fill = "#f5f7fd")) + 
  xlim(1990, 2006) + 
  scale_color_manual(values = colors)

plot_struck_off <- ggplot(entities_lg_blv) + 
  geom_bar(aes(x = struck_off_year), fill = "#F0303E", color = "white") +
  ggthemes::theme_fivethirtyeight() + 
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black")) + 
  xlim(1985, 2010)
  
  geom_line(data = entities_struck[1:19, ], aes(x = struck_off_year, y = n), color = "darkorange") + 
  geom_line(data = entities_incorp, aes(x = incorporation_year, y = n)) + 
  ggthemes::theme_fivethirtyeight() + 
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black")) + 
  xlim(1985, 2010)
  
  
# Average longevity of struck-off entities # 
avg <- entities_lg_blv %>% 
  filter(!struck_off_year == "") %>% 
  mutate(longevity = (struck_off_year - incorporation_year)) %>% 
  filter(struck_off_year < 2000)

mean(avg$longevity)

# ---------------------------------------------------------------- # 
# Explore Officers Connected to Entities Registered at Address
# --------------------------------------------------------------- # 

# Extract relationships
relationships_ent <- relationships %>% 
  filter(node_id_end %in% c(entities_lg_blv$node_id)) %>% 
  filter(rel_type %in% c("officer_of", "intermediary_of"))

unique(relationships_ent$node_id_start)

# Year of start 
relationships_ent$start_format <- anytime(relationships_ent$start_date)
relationships_ent$start_year <- as.numeric(format(relationships_ent$start_format , "%Y"))

# Officers start
off_start <- relationships_ent %>% 
  group_by(start_year) %>% 
  tally()


# Plot
ggplot() + 
  geom_line(data = off_start[1:27, ], aes(y = n , x = start_year, color = "Start Year")) + 
  geom_ribbon(data = off_start[1:27, ], aes(y = n, x = start_year, ymin = 0, ymax = n), fill = "darkgreen", alpha = 0.3) +
  geom_point(data = off_start[1:27, ], aes(y = n, x = start_year), color = "darkgreen", size = 7) +
  geom_text(data = off_start[1:27, ], aes(y = n, x = start_year, label = n,),
            color = "white", fontface = "bold", alpha = 2, size = 4) +
  ggthemes::theme_fivethirtyeight() + 
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(fill = "#f5f7fd"),
        panel.background = element_rect(fill = "#f5f7fd")) + 
  xlim(1990, 2006) + 
  scale_color_manual(values = c("Start Year" = "darkgreen"))


