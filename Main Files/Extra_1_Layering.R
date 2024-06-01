# Layer Figures # 

# Layering for key tax havens. 
# Runned in conjuction with 4_Geospatial_Analysis. 

# Packages
library(tidyverse)
library(osmdata)
library(RColorBrewer)
library(spatstat)
library(sf)

# Coords for world
coordinates_all$coords <- tibble(longitude = coordinates_all$lon, latitude = coordinates_all$lat)

coordinates_all$coords_sf <- st_as_sf(coordinates_all$coords,
                                  coords = c("longitude", "latitude"),
                                  crs = 4326)

coordinates_all$eters_laea <- st_transform(coordinates_all$coords_sf, 4326)

coordinates <- coordinates_all

# Function for rotation sf 
rotate_sf <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function (x) { 
    matrix(c(2, 1.2, 0, 1), 2, 2) 
  }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() * rotate_matrix(pi / 20) + c(x_add, y_add)
    )
}

# Query OSM data for roads in Medellin
osm_medellin <- getbb("Medellín Colombia") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

# Extract roads 
medellin_roads <- st_intersection(osm_medellin$osm_lines, medellin_pol)

# Extract coords
medellin_sf <- st_as_sf(medellin_pol$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

bahamas_sf <- st_make_valid(medellin_sf)

# Coords
med_coords <- st_filter(points_sf, medellin_sf)

med_coords$eters_laea <- st_transform(med_coords, 4326)

# Illustrative 
ggplot() +
  geom_sf(data = rotate_sf(medellin_pol, y_add = 1), fill = "white", color = "white") +
  geom_sf(data = rotate_sf(medellin_roads, y_add = 1)) +
  geom_sf(data = rotate_sf(medellin_pol, y_add = 1.1), fill = "white") +
  geom_sf(data = rotate_sf(medellin_pol, y_add = 1.2), fill = custom_palette(medellin_pol$within), alpha = 5,
          color = "white",
          aes(alpha = 0.2)) +
  geom_sf(data = rotate_sf(med_coords$eters_laea, y_add = 1.3),
          size = 0.5) +
  geom_sf(data = rotate_sf(medellin_pol, y_add = 1.3), fill = "grey", color = "grey", alpha = 0.05) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey95", color = "transparent"))

# Map
ggplot() + 
  geom_sf(data = medellin_pol, fill = "white", color = "black", size = 100) + 
  geom_sf(data = medellin_roads, alpha = 0.3) + 
  geom_sf(data = medellin_pol, fill = custom_palette(medellin_pol$within), alpha = 0.5, color = "black", size = 100) + 
  geom_sf(data = med_coords$eters_laea, size = 0.5) +
  theme_void()

med_coords$coordinates <- st_coordinates(med_coords)

med_coords$lon <- med_coords$coordinates[, "X"]
med_coords$lat <- med_coords$coordinates[, "Y"]

# Adding heat-map layer
ggplot(med_coords, aes(x = lon, y = lat)) +
  stat_density2d(
    aes(fill = ..level..),
    contour_var = "ndensity",
    alpha = 1/3,
    geom = "polygon",
    h = 0.01) + 
  scale_fill_gradientn(colors = c("blue", "orange")) + 
  geom_sf(data = medellin_pol, fill = "white", color = "black", size = 100, alpha = 0.2) + 
  geom_sf(data = medellin_roads, alpha = 0.2) + 
  geom_sf(data = medellin_pol, fill = custom_palette(medellin_pol$within), alpha = 0.2, color = "black", size = 100) +
    theme_void() +
  geom_sf(data = med_coords, size = 0.5, alpha = 1/3) + 
  theme(legend.position = "none")

# ------------------------- # 
# Layers for Maps in Thesis # 
# ------------------------- # 

# ----- # 
# Colombia # 
# ----- # 

# Coords
col_sf <- st_as_sf(states$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

col_sf <- st_make_valid(col_sf)

# Coords
col_coords <- st_filter(points_sf, col_sf)

col_coords$eters_laea <- st_transform(col_coords, 4326)

# Layers for Colombia 
ggplot() +
  geom_sf(data = rotate_sf(states, y_add = 11), fill = "white") +
  geom_sf(data = rotate_sf(states, y_add = 22), fill = custom_palette(states$within), alpha = 2,
          color = "white",
          aes(alpha = 0.2)) +
  geom_sf(data = rotate_sf(col_coords$eters_laea, y_add = 33),
          size = 0.2) + 
  geom_sf(data = rotate_sf(states, y_add = 33), fill = "grey", color = "grey", alpha = 0.2) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

# ----- # 
# Bogota # 
# ------ # 

# Bogota-Coords
bogota_sf <- st_as_sf(bogota_pol$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

bogotasf <- st_make_valid(bogota_sf)

# Coords
bog_coords <- st_filter(points_sf, bogota_sf)

bog_coords$eters_laea <- st_transform(bog_coords, 4326)

# Layers for Bogota 
ggplot() + 
  geom_sf(data = rotate_sf(bogota_pol, y_add = 1.2), fill = "white", color = "black") +
  geom_sf(data = rotate_sf(bogota_pol, y_add = 1.4), fill = custom_palette(bogota_pol$within), alpha = 2,
          color = "white",
          aes(alpha = 0.2)) +
  geom_sf(data = rotate_sf(bog_coords$eters_laea, y_add = 1.6),
          size = 0.5) + 
  geom_sf(data = rotate_sf(bogota_pol, y_add = 1.6), fill = "grey", color = "grey", alpha = 0.2) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

# ----- # 
# Medellin # 
# ------ # 

# Medellin-cords
medellin_sf <- st_as_sf(medellin_pol$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

medellin_sf <- st_make_valid(medellin_sf)

# Coords
med_coords <- st_filter(points_sf, medellin_sf)

med_coords$eters_laea <- st_transform(med_coords, 4326)

# Layers for Medellin 
ggplot() + 
  geom_sf(data = rotate_sf(medellin_pol, y_add = 1.1), fill = "white", color = "black") +
  geom_sf(data = rotate_sf(medellin_pol, y_add = 1.2), fill = custom_palette(medellin_pol$within), alpha = 2,
          color = "white",
          aes(alpha = 0.2)) +
  geom_sf(data = rotate_sf(med_coords$eters_laea, y_add = 1.3),
          size = 0.5) + 
  geom_sf(data = rotate_sf(medellin_pol, y_add = 1.3), fill = "grey", color = "grey", alpha = 0.2) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

# ----- # 
# World  # 
# ------ # 
ggplot() + 
  geom_sf(data = rotate_sf(world2, y_add = 1), fill = "white", color = "black") + 
  geom_sf(data = rotate_sf(world2, y_add = 100), fill = custom_palette(world2$nr_coordinates), alpha = 20,
          color = "white",
          aes(alpha = 0.2))  +
  geom_sf(data = rotate_sf(coordinates$eters_laea, y_add = 200),
          size = 0.5) +
  geom_sf(data = rotate_sf(world2, y_add = 200), fill = "grey", color = "grey", alpha = 0.2) +  
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))


# Globus



# ----- # 
# Bahamas # 
# ------ # 
# Coords
# Extract coords 
bahamas_sf <- st_as_sf(bahamas$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

bahamas_sf <- st_make_valid(bahamas_sf)

# Coords
bah_coords <- st_filter(points_sf, bahamas_sf)

bah_coords$eters_laea <- st_transform(bah_coords, 4326)


# Layers
ggplot() + 
  geom_sf(data = rotate_sf(bahamas, y_add = 5), fill = "white", color = "black") +
  geom_sf(data = rotate_sf(bahamas, y_add = 10), fill = custom_palette(bahamas$within), alpha = 25,
          color = "white",
          aes(alpha = 0.2)) +
  geom_sf(data = rotate_sf(bah_coords, y_add = 15),
          size = 0.5) +
  geom_sf(data = rotate_sf(bahamas, y_add = 15), fill = "grey", color = "grey", alpha = 0.01) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : New Providence
############ 
new_prov <- st_read("JSON/nassau.geojson")
new_prov <- st_union(new_prov)

# Coordinates 
new_prov_coords <- st_filter(points_sf, new_prov)
new_prov_coords$coordinates <- st_coordinates(new_prov_coords)

new_prov_coords$lon <- new_prov_coords$coordinates[, "X"]
new_prov_coords$lat <- new_prov_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
osm_new_providence <- getbb("New Providence, The Bahamas") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Cities
cities <- getbb("New Providence, The Bahamas") %>%
  opq() %>% 
  add_osm_feature(key = "place",
                  value = c("city", "borough", "suburb")) %>% 
  osmdata_sf()

# Forest
forest <- getbb("New Providence, The Bahamas") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood")) %>% 
  osmdata_sf()

# Water
water <-  getbb("New Providence, The Bahamas") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water", "wetland")) %>% 
  osmdata_sf()

# Extract for plotting
new_prov_roads <- st_intersection(osm_new_providence$osm_lines, new_prov)
new_prov_forrest <- st_intersection(forest$osm_polygons, new_prov)
new_prov_water <- st_intersection(water$osm_polygons, new_prov)

# Map
ggplot() + 
  geom_sf(data = new_prov, fill = "white", color = "black") +
  geom_sf(data = new_prov_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = new_prov_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = new_prov_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = new_prov_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = new_prov_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.7, contour_var = "density", breaks = c(3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

# ----- # 
# Aruba # 
# ------ # 

# Extract coords 
aruba_sf <- st_as_sf(aruba$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

aruba_sf <- st_make_valid(aruba_sf)

# Coords
awb_coords <- st_filter(points_sf, aruba_sf)

awb_coords$eters_laea <- st_transform(awb_coords$x, 4326)

# Layers
ggplot() + 
  geom_sf(data = rotate_sf(aruba, y_add = 1), fill = "white", color = "black") +
  geom_sf(data = rotate_sf(aruba, y_add = 1.1), fill = custom_palette(aruba$within), alpha = 25,
          color = "white",
          aes(alpha = 0.2)) + 
  geom_sf(data = rotate_sf(awb_coords, y_add = 1.2),
          size = 0.1) +
  geom_sf(data = rotate_sf(aruba, y_add = 1.2), fill = "grey", color = "grey", alpha = 0.01) + 
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))


############ 
# For Appendix : Aruba
############ 
awb_coords$coordinates <- st_coordinates(awb_coords)

awb_coords$lon <- awb_coords$coordinates[, "X"]
awb_coords$lat <- awb_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Aruba") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Cities
cities <- getbb("Aruba") %>%
  opq() %>% 
  add_osm_feature(key = "place",
                  value = c("city", "borough", "suburb")) %>% 
  osmdata_sf()

# Forest
forest <- getbb("Aruba") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Aruba") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water", "wetland")) %>% 
  osmdata_sf()

# Extract for plotting
aruba_roads <- st_intersection(roads$osm_lines, aruba_sf)
aruba_forrest <- st_intersection(forest$osm_polygons, aruba_sf)
aruba_water <- st_intersection(water$osm_polygons, aruba_sf)

# Map
ggplot() + 
  geom_sf(data = aruba_sf, fill = "white", color = "black") +
  geom_sf(data = aruba_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = awb_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = aruba_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = aruba_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = awb_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.6, contour_var = "density", 
                        breaks = c(200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))


# ----- # 
# Barbados # 
# ------ # 

# 
osm_barbados <- getbb("Barbados") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

# Extract coords 
barbados_sf <- st_as_sf(barbados$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

barbados_sf <- st_make_valid(barbados_sf)

# Coords
bar_coords <- st_filter(points_sf, barbados_sf)

bar_coords$eters_laea <- st_transform(bar_coords$x, 4326)


# Layers
ggplot() + 
  geom_sf(data = rotate_sf(barbados, y_add = 1), fill = "white", color = "black") +
  geom_sf(data = rotate_sf(barbados, y_add = 1.15), fill = custom_palette(barbados$within), alpha = 25,
          color = "white",
          aes(alpha = 0.2)) + 
  geom_sf(data = rotate_sf(osm_barbados$osm_lines, y_add = 1), alpha = 0.2) + 
  geom_sf(data = rotate_sf(bar_coords, y_add = 1.3),
          size = 0.1) +
  geom_sf(data = rotate_sf(barbados, y_add = 1.3), fill = "grey", color = "grey", alpha = 0.01) + 
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

ggplot() + 
  geom_sf(data = barbados , fill = "white", color = "black") +
  geom_sf(data = barbados, fill = custom_palette(barbados$within), alpha = 25,
          color = "white",
          aes(alpha = 0.2)) + 
  geom_sf(data = osm_barbados$osm_lines, alpha = 0.2) + 
  geom_sf(data = bar_coords,
          size = 0.1) +
  geom_sf(data = barbados, fill = "grey", color = "grey", alpha = 0.01) + 
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Barbados
############ 
bar_coords$coordinates <- st_coordinates(bar_coords)

bar_coords$lon <- bar_coords$coordinates[, "X"]
bar_coords$lat <- bar_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Barbados") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Cities
cities <- getbb("Barbados") %>%
  opq() %>% 
  add_osm_feature(key = "place",
                  value = c("city", "borough", "suburb")) %>% 
  osmdata_sf()

# Forest
forest <- getbb("Barbados") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Barbados") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water", "wetland")) %>% 
  osmdata_sf()

# Extract for plotting
bar_roads <- st_intersection(roads$osm_lines, barbados_sf)
bar_forrest <- st_intersection(forest$osm_polygons, barbados_sf)
bar_water <- st_intersection(water$osm_polygons, barbados_sf)

# Map
ggplot() + 
  geom_sf(data = barbados_sf, fill = "white", color = "black") +
  geom_sf(data = bar_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = bar_coords, alpha = 0.5, fill = "white",color = "black", size = 0.5) +
  geom_sf(data = bar_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = bar_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = bar_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.6, contour_var = "density",
                        breaks = c(400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))



# ----- # 
# Bermuda # 
# ------ # 
# Coords
# Extract coords 
bermuda_sf <- st_as_sf(bermuda$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

bermuda_sf <- st_make_valid(bermuda_sf)

# Coords
ber_coords <- st_filter(points_sf, bermuda_sf)

ber_coords$eters_laea <- st_transform(ber_coords, 4326)


# Layers
ggplot() + 
  geom_sf(data = rotate_sf(bermuda, y_add = 1), fill = "white", color = "black") +
  geom_sf(data = rotate_sf(bermuda, y_add = 1.05), fill = custom_palette(bermuda$within), alpha = 25,
          color = "white",
          aes(alpha = 0.2)) +
  geom_sf(data = rotate_sf(ber_coords, y_add = 1.1),
          size = 0.5) +
  geom_sf(data = rotate_sf(bermuda, y_add = 1.1), fill = "grey", color = "grey", alpha = 0.01) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Bermuda
############ 
ber_coords$coordinates <- st_coordinates(ber_coords)

ber_coords$lon <- ber_coords$coordinates[, "X"]
ber_coords$lat <- ber_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Bermuda") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Cities
cities <- getbb("Bermuda") %>%
  opq() %>% 
  add_osm_feature(key = "place",
                  value = c("city", "borough", "suburb")) %>% 
  osmdata_sf()

# Forest
forest <- getbb("Bermuda") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Bermuda") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water", "wetland")) %>% 
  osmdata_sf()

# Extract for plotting
ber_roads <- st_intersection(roads$osm_lines, bermuda_sf)
ber_forrest <- st_intersection(forest$osm_polygons, bermuda_sf)
ber_water <- st_intersection(water$osm_polygons, bermuda_sf)

# Map
ggplot() + 
  geom_sf(data = bermuda_sf, fill = "white", color = "black") +
  geom_sf(data = ber_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = ber_coords, alpha = 0.5, fill = "white",color = "black", size = 0.5) +
  geom_sf(data = ber_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = ber_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = ber_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.6, contour_var = "density",
                        breaks = c(3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

# ----- # 
# Panama # 
# ------ # 
# Coords
# Extract coords 
panama_sf <- st_as_sf(panama$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

panama_sf <- st_make_valid(panama_sf)

# Coords
pan_coords <- st_filter(points_sf, panama_sf)

pan_coords$eters_laea <- st_transform(pan_coords, 4326)


# Layers
ggplot() + 
  geom_sf(data = rotate_sf(panama, y_add = 1), fill = "white", color = "black") +
  geom_sf(data = rotate_sf(panama, y_add = 3), fill = custom_palette(panama$within), alpha = 25,
          color = "white",
          aes(alpha = 0.2)) +
  geom_sf(data = rotate_sf(pan_coords, y_add = 5),
          size = 0.5) +
  geom_sf(data = rotate_sf(panama, y_add = 5), fill = "grey", color = "grey", alpha = 0.01) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Panama
############ 

# Panama City 
panama_subregions <- st_read("JSON/panama_corregimientos.geojson")

panama_city <- panama_subregions %>% 
  filter(Distrito %in% c("Panamá", "San Miguelito")) %>% 
  filter(!Corregimiento %in% c("Caimitillo", "Pacora", "Las Garzas", "San Martín", "24 de Diciembre", "Tocumen"))

panama_city2 <- panama_subregions %>% 
  filter(Corregimiento %in% c("Nuevo Emperador", "Burunga", "Arraiján (Cabecera)","Veracruz"))

panama_city <- rbind(panama_city, panama_city2)


panama_city_sf <- st_make_valid(panama_city$geometry)

panama_city_sf <- st_transform(panama_city_sf, "+proj=longlat +datum=WGS84")

ggplot() + 
  geom_sf(data = panama_city_sf)

# Coordinates 
pan_coords <- st_filter(points_sf, panama_city_sf)

pan_coords$coordinates <- st_coordinates(pan_coords)

pan_coords$lon <- pan_coords$coordinates[, "X"]
pan_coords$lat <- pan_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Panama Distrito") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("Panama Distrito") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Panama Distrito") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water", "wetland")) %>% 
  osmdata_sf()

# Extract for plotting
pan_roads <- st_intersection(roads$osm_lines, panama_city_sf)
pan_forrest <- st_intersection(forest$osm_polygons, panama_city_sf)
pan_water <- st_intersection(water$osm_polygons, panama_city_sf)

# Map
ggplot() + 
  geom_sf(data = panama_city_sf, fill = "white", color = "black") +
  geom_sf(data = pan_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = pan_coords, alpha = 0.5, fill = "white",color = "black", size = 0.5) +
  geom_sf(data = pan_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = pan_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = pan_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.6, contour_var = "density",
                        breaks= c(200, 400, 600, 800, 1000, 1200,1400, 1600, 1800, 2000, 2200, 2400, 2600, 2800)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))


# ----- # 
# Malta # 
# ------ # 
# Coords
# Extract coords 
malta_sf <- st_as_sf(malta$geometry)
points_sf <- st_as_sf(coordinates$eters_laea)

malta_sf <- st_make_valid(malta_sf)

# Coords
mal_coords <- st_filter(points_sf, malta_sf)

mal_coords$eters_laea <- st_transform(mal_coords, 4326)


# Layers
ggplot() + 
  geom_sf(data = rotate_sf(malta, y_add = 1), fill = "white", color = "black") +
  geom_sf(data = rotate_sf(malta, y_add = 1.25), fill = custom_palette(malta$within), alpha = 25,
          color = "white",
          aes(alpha = 0.2)) +
  geom_sf(data = rotate_sf(mal_coords, y_add = 1.5),
          size = 0.5) +
  geom_sf(data = rotate_sf(malta, y_add = 1.5), fill = "grey", color = "grey", alpha = 0.01) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Bermuda
############ 
mal_coords$coordinates <- st_coordinates(mal_coords)

mal_coords$lon <- mal_coords$coordinates[, "X"]
mal_coords$lat <- mal_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Malta") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("Malta") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Malta") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water", "wetland")) %>% 
  osmdata_sf()

# Extract for plotting
mal_roads <- st_intersection(roads$osm_lines, malta_sf)
mal_forrest <- st_intersection(forest$osm_polygons, malta_sf)
mal_water <- st_intersection(water$osm_polygons, malta_sf)

# Map
ggplot() + 
  geom_sf(data = malta_sf, fill = "white", color = "black") +
  geom_sf(data = mal_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = mal_coords, alpha = 0.5, fill = "white",color = "black", size = 0.5) +
  geom_sf(data = mal_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = mal_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = mal_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.3, contour_var = "density",
                        breaks = c(200, 400, 600, 800, 1000, 1200, 1400, 1600)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Bermuda
############ 
mal_coords$coordinates <- st_coordinates(mal_coords)

mal_coords$lon <- mal_coords$coordinates[, "X"]
mal_coords$lat <- mal_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Malta") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("Malta") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Malta") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water", "wetland")) %>% 
  osmdata_sf()

# Extract for plotting
mal_roads <- st_intersection(roads$osm_lines, malta_sf)
mal_forrest <- st_intersection(forest$osm_polygons, malta_sf)
mal_water <- st_intersection(water$osm_polygons, malta_sf)

# Map
ggplot() + 
  geom_sf(data = malta_sf, fill = "white", color = "black") +
  geom_sf(data = mal_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = mal_coords, alpha = 0.5, fill = "white",color = "black", size = 0.5) +
  geom_sf(data = mal_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = mal_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = mal_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.3, contour_var = "density",
                        breaks = c(200, 400, 600, 800, 1000, 1200, 1400, 1600)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Cayman Islands
############ 
caymans <- st_read("JSON/caymans.geojson")

cay_coords <- st_filter(points_sf, caymans)

cay_coords$coordinates <- st_coordinates(cay_coords)

cay_coords$lon <- cay_coords$coordinates[, "X"]
cay_coords$lat <- cay_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Grand Cayman") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("Grand Cayman") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Grand Cayman") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
cay_roads <- st_intersection(roads$osm_lines, caymans)
cay_forrest <- st_intersection(forest$osm_polygons, caymans)
cay_water <- st_intersection(water$osm_polygons, caymans)

# Map
ggplot() + 
  geom_sf(data = caymans, fill = "white", color = "black") +
  geom_sf(data = cay_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = cay_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = cay_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = cay_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = cay_coords, aes(x = lon, y = lat),
                        h = c(0.1, 0.1), alpha = 0.6, contour_var = "ndensity", geom = "polygon") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Geneva
############ 

# Polygon
geneva <- st_read("JSON/geneva.geojson")
geneva <- st_union(geneva)

# Geneva coordinates
geneva_coords <- st_filter(points_sf, geneva)

geneva_coords$coordinates <- st_coordinates(geneva_coords)

geneva_coords$lon <- geneva_coords$coordinates[, "X"]
geneva_coords$lat <- geneva_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Genevra, Switzerland") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

ggplot() + 
  geom_sf(data = roads$osm_lines)

# Forest
forest <- getbb("Canton de Genève") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Canton de Genève") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
gen_roads <- st_intersection(roads$osm_lines, geneva)
gen_forrest <- st_intersection(forest$osm_polygons, geneva)
gen_water <- st_intersection(water$osm_polygons, geneva)

# Map
ggplot() + 
  geom_sf(data = geneva, fill = "white", color = "black") +
  geom_sf(data = gen_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = geneva_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = gen_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = gen_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = geneva_coords, aes(x = lon, y = lat),
                        h = c(0.04, 0.04), alpha = 0.5, contour_var = "ndensity",
                        breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))


############ 
# For Appendix : Jersey
############ 
jersey <- st_read("JSON/jersey2.geojson")

jersey <- jersey_initial %>% 
  slice(1, 6)

jersey <- st_union(jersey)

jersey_sf <- st_make_valid(jersey)

jersey_coords <- st_filter(points_sf, jersey)

        
jersey_coords$coordinates <- st_coordinates(jersey_coords)

jersey_coords$lon <- jersey_coords$coordinates[, "X"]
jersey_coords$lat <- jersey_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Bailiwick of Jersey") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

ggplot() + 
  geom_sf(data = roads$osm_lines)

# Forest
forest <- getbb("Bailiwick of Jersey") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Bailiwick of Jersey") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
jer_roads <- st_intersection(roads$osm_lines, jersey)
jer_forrest <- st_intersection(forest$osm_polygons, jersey)
jer_water <- st_intersection(water$osm_polygons, jersey)

# Map
ggplot() + 
  geom_sf(data = jersey, fill = "white", color = "black") +
  geom_sf(data = jer_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = jersey_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = jer_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = jer_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = jersey_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.5, contour_var = "ndensity",
                        breaks = c( 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))


############ 
# For Appendix : Venezuela
############ 

# Polygons
venezuela <- st_read("JSON/venezuela.geojson")

venezuela <- venezuela %>% 
  filter(shapeName %in% c("Baruta", "Libertador", "EL Hatillo", "Sucre", "Chacao"))

venezuela <- venezuela[c(1, 2, 3, 8, 18), ]

ggplot() + 
  geom_sf(data = venezuela)


ven_coords <- st_filter(points_sf, venezuela)


ven_coords$coordinates <- st_coordinates(ven_coords)

ven_coords$lon <- ven_coords$coordinates[, "X"]
ven_coords$lat <- ven_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Caracas Venezuela") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("Caracas Venezuela") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Caracas Venezuela") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
ven_roads <- st_intersection(roads$osm_lines, venezuela)
ven_forrest <- st_intersection(forest$osm_polygons, venezuela)
ven_water <- st_intersection(water$osm_polygons, venezuela)

# Map
ggplot() + 
  geom_sf(data = venezuela, fill = "white", color = "black") +
  geom_sf(data = ven_roads, alpha = 0.2, fill = "white", color = "black") +
  geom_sf(data = ven_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = ven_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = ven_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = ven_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.5, contour_var = "ndensity",
                        breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Peru, Lima
############ 

peru <- ne_states(country = "Peru")

peru <- peru %>% 
  filter(name %in% c("Lima Province"))

peru_coords <- st_filter(points_sf, peru)

peru_coords$coordinates <- st_coordinates(peru_coords)

peru_coords$lon <- peru_coords$coordinates[, "X"]
peru_coords$lat <-peru_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Lima") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("Lima") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Lima") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
peru_roads <- st_intersection(roads$osm_lines, peru)
peru_forrest <- st_intersection(forest$osm_polygons, peru)
peru_water <- st_intersection(water$osm_polygons, peru)

# Map
ggplot() + 
  geom_sf(data = peru, fill = "white", color = "black") +
  geom_sf(data = peru_roads, alpha = 0.05, fill = "white", color = "black") +
  geom_sf(data = peru_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = peru_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = peru_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = peru_coords, aes(x = lon, y = lat),
                        h = c(0.05, 0.05), alpha = 0.5, contour_var = "ndensity",
                        breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Quito, Ecuador
############ 
ecuador <- st_read("JSON/ecuador.geojson")

ecuador <- ecuador %>% 
  filter(shapeName == "Quito")

ggplot() + 
  geom_sf(data = ecuador)

# Coordinates
ecu_coords <- st_filter(points_sf, ecuador)

ggplot() + 
  geom_sf(data = ecuador) + 
  geom_sf(data = ecu_coords)


ecu_coords$coordinates <- st_coordinates(ecu_coords)

ecu_coords$lon <- ecu_coords$coordinates[, "X"]
ecu_coords$lat <-ecu_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("San Francisco de Quito") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

ggplot() + 
  geom_sf(data = roads$osm_lines)

# Forest
forest <- getbb("San Francisco de Quito") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("San Francisco de Quito") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
ecu_roads <- st_intersection(roads$osm_lines, ecuador)
ecu_forrest <- st_intersection(forest$osm_polygons, ecuador)
ecu_water <- st_intersection(water$osm_polygons, ecuador)

# Map
ggplot() + 
  geom_sf(data = ecuador, fill = "white", color = "black") +
  geom_sf(data = ecu_roads, alpha = 0.075, fill = "white", color = "black") +
  geom_sf(data = ecu_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = ecu_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = ecu_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = ecu_coords, aes(x = lon, y = lat),
                        h = c(0.05, 0.05), alpha = 0.5, contour_var = "ndensity",
                        breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Guatemala 
############ 
guatemala <- st_read("JSON/guatemala.geojson")
guatemala <- st_make_valid(guatemala)

guatemala <- guatemala %>% 
  filter(shapeName %in% c("Guatemala"))

# Coordinates
gua_coords <- st_filter(points_sf, guatemala)

ggplot() + 
  geom_sf(data = guatemala) + 
  geom_sf(data = gua_coords)


gua_coords$coordinates <- st_coordinates(gua_coords)

gua_coords$lon <- gua_coords$coordinates[, "X"]
gua_coords$lat <-gua_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Guatemala City") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

ggplot() + 
  geom_sf(data = roads$osm_lines)

# Forest
forest <- getbb("Guatemala City") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Guatemala City") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
gua_roads <- st_intersection(roads$osm_lines, guatemala)
gua_forrest <- st_intersection(forest$osm_polygons, guatemala)
gua_water <- st_intersection(water$osm_polygons, guatemala)

# Map
ggplot() + 
  geom_sf(data = guatemala, fill = "white", color = "black") +
  geom_sf(data = gua_roads, alpha = 0.075, fill = "white", color = "black") +
  geom_sf(data = gua_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = gua_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = gua_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = gua_coords, aes(x = lon, y = lat),
                        h = c(0.02, 0.02), alpha = 0.5, contour_var = "ndensity",
                        breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

############ 
# For Appendix : Montevideo
############ 

uruguay <- ne_states(country = "Uruguay")

uruguay <- uruguay %>% 
  filter(name == "Montevideo")

ggplot() + 
  geom_sf(data = uruguay)

# Coordinates
uruguay_coords <- st_filter(points_sf, uruguay)

ggplot() + 
  geom_sf(data = uruguay) + 
  geom_sf(data = uruguay_coords)


uruguay_coords$coordinates <- st_coordinates(uruguay_coords)

uruguay_coords$lon <- uruguay_coords$coordinates[, "X"]
uruguay_coords$lat <- uruguay_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Montevideo") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("Montevideo") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Montevideo") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
uru_roads <- st_intersection(roads$osm_lines, uruguay)
uru_forrest <- st_intersection(forest$osm_polygons, uruguay)
uru_water <- st_intersection(water$osm_polygons, uruguay)

# Map
ggplot() + 
  geom_sf(data = uruguay, fill = "white", color = "black") +
  geom_sf(data = uru_roads, alpha = 0.25, fill = "white", color = "black") +
  geom_sf(data = uruguay_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = uru_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = uru_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = uruguay_coords, aes(x = lon, y = lat),
                        h = c(0.015, 0.015), alpha = 1, contour_var = "ndensity",
                        breaks = c( 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))



############ 
# For Appendix : BVI
############ 

# Coordinates
vgb_coords <- st_filter(points_sf, vgb)

ggplot() + 
  geom_sf(data = vgb) + 
  geom_sf(data = vgb_coords)


vgb_coords$coordinates <- st_coordinates(vgb_coords)

vgb_coords$lon <- vgb_coords$coordinates[, "X"]
vgb_coords$lat <- vgb_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("British Virgin Islands") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("British Virgin Islands") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("British Virgin Islands") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
vgb_roads <- st_intersection(roads$osm_lines, vgb)
vgb_forrest <- st_intersection(forest$osm_polygons, vgb)
vgb_water <- st_intersection(water$osm_polygons, vgb)

# Map
ggplot() + 
  geom_sf(data = vgb, fill = "white", color = "black") +
  geom_sf(data = vgb_roads, alpha = 0.075, fill = "white", color = "black") +
  geom_sf(data = vgb_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = vgb_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = vgb_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = vgb_coords, aes(x = lon, y = lat),
                        h = c(0.015, 0.015), alpha = 1, contour_var = "ndensity",
                        breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))


############ 
# For Appendix : Miami
############ 

miami <- st_read("JSON/miami.geojson")

# Coordinates
miami_coords <- st_filter(points_sf, miami)

miami_coords$coordinates <- st_coordinates(miami_coords)

miami_coords$lon <- miami_coords$coordinates[, "X"]
miami_coords$lat <- miami_coords$coordinates[, "Y"]

# Roads, ports and cities 
# Roads
roads <- getbb("Miami") %>%
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("trunk", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

# Forest
forest <- getbb("Miami") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("grassland", "grassland", "heath", "moor", "scrub", "shrubbery", "tundra", "wood", "wetland")) %>% 
  osmdata_sf()

# Water
water <-  getbb("Miami") %>%
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# Extract for plotting
mia_roads <- st_intersection(roads$osm_lines, miami)
mia_forrest <- st_intersection(forest$osm_polygons, miami)
mia_water <- st_intersection(water$osm_polygons, miami)

# Map
ggplot() + 
  geom_sf(data = miami, fill = "white", color = "black") +
  geom_sf(data = mia_roads, alpha = 0.4, fill = "white", color = "black") +
  geom_sf(data = miami_coords, alpha = 0.5, fill = "white",color = "black", size = 1) +
  geom_sf(data = mia_forrest, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = mia_water, color = "lightblue", fill = "lightblue") + 
  stat_density2d_filled(data = miami_coords, aes(x = lon, y = lat),
                        h = c(0.01, 0.01), alpha = 1, contour_var = "ndensity",
                        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "transparent"))

