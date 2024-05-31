# Drug Routes # 

# Loading libraries 
library(geosphere)
library(leaflet.extras2)
library(tidyverse)

# Loading routes 
cocaine_flows <- read.csv("CSV-files/Coca_Data/cocaine_flows.csv", sep = ";")

cocaine_flows <- cocaine_flows[1:44, ]

# --------------- # 
# Preparing data  # 
# --------------- # 

# Cocaine Flows 
flows <- data.frame(
  port_lat = as.numeric(c(cocaine_flows$Port_lat)),
  port_lng = as.numeric(c(cocaine_flows$Port_lng)),
  receiver_lat = as.numeric(c(cocaine_flows$Receiver_lat)),
  receiver_lng = as.numeric(c(cocaine_flows$Receiver_lng)),
  weight = as.numeric(c(cocaine_flows$Flow_Factor)),
  color = cocaine_flows$Color
)

flows <- flows[1:44, ]


# Points based on nr of routes 
points_port <- cocaine_flows %>% 
  mutate(lat = Port_lat, lng = Port_lng) %>% 
  group_by(lat, lng) %>% 
  tally()

points_receiver <- cocaine_flows %>% 
  mutate(lat = Receiver_lat, lng = Receiver_lng) %>% 
  group_by(lat, lng) %>% 
  tally()

points <- rbind(points_port, points_receiver)

points <- points %>% 
  group_by(lat, lng) %>% 
  mutate(tot = sum(n))

# Labels 
labels_1 <- cocaine_flows %>% 
  filter(Label == 1 & Port_Label == 0) %>% 
  mutate(type = Receiver, lng = Receiver_lng, lat = Receiver_lat) %>% 
  select(type, lng, lat) %>% 
  distinct(type, lng, lat)

labels_2 <- cocaine_flows %>% 
  filter(Label == 1 & Port_Label == 1) %>% 
  mutate(type = Port,lng = Port_lng, lat = Port_lat) %>% 
  select(type, lng, lat) %>% 
  distinct(type, lng, lat)

labels <- rbind(labels_1, labels_2)

# --------------- # 
# -- Mapping  --- # 
# --------------- # 
m <- leaflet(options = leafletOptions(zoomControl = FALSE, backgroundColor = "#FFFFFF")) %>%
  addProviderTiles(providers$CartoDB) 

drug_routes_list <- list()

# Loop over each row in the dataframe
for (i in 1:nrow(flows)) {
  # Extract the coordinates for the current row
  port_lat <- flows[i, "port_lat"]
  port_lng <- flows[i, "port_lng"]
  receiver_lat <- flows[i, "receiver_lat"]
  receiver_lng <- flows[i, "receiver_lng"]
  
  # Create dataframe for current drug route
  drug_route <- data.frame(
    lon = c(port_lng, receiver_lng),
    lat = c(port_lat, receiver_lat)
  )
  
  # Add drug route dataframe to the list
  drug_routes_list[[i]] <- drug_route
}

for (i in 1:length(drug_routes_list)) {
  m <- m %>%
    addGeodesicPolylines(data = drug_routes_list[[i]], lng = ~lon, lat = ~lat, color = flows$color[i], weight = flows$weight[i]/3, 
                         opacity = 0.5)
}

for (i in 1:length(drug_routes_list)) {
  world_map <- world_map %>%
    addGeodesicPolylines(data = drug_routes_list[[i]], lng = ~lon, lat = ~lat, color = flows$color[i], weight = flows$weight[i]/3, 
                         opacity = 0.5)
}



m

m %>% 
  addCircleMarkers(data = points,
                   lng = ~lng,
                   lat = ~lat,
                   radius = ~tot,
                   weight = 5,
                   fillOpacity = 1)

world_map %>% 
  addCircleMarkers(data = points,
                   lng = ~lng,
                   lat = ~lat,
                   radius = ~tot,
                   weight = 5,
                   fillOpacity = 1) %>% 
  addLabelOnlyMarkers(data = labels, lng = ~lng, lat = ~lat, label = ~as.character(type),
                      labelOptions = labelOptions(noHide = T, direction = "top", textOnly = T,
                                                  style = list("font-size" = "12px",
                                                  "font-weight" = "bold")))



# testing 
flows_caribbean <- read.csv("CSV-files/Coca_Data/Cocaine_Flows_Caribbean.csv", sep = ";", nrows = 17)


# test 
drug_routes <- list()

# Loop over each row in the dataframe
for (i in 1:nrow(flows_caribbean)) {
  # Extract the coordinates for the current row
  port_lat <- flows_caribbean[i, "Port_lat"]
  port_lng <- flows_caribbean[i, "Port_lon"]
  receiver_lat <- flows_caribbean[i, "Receiver_lat"]
  receiver_lng <- flows_caribbean[i, "Receiver_lng"]
  
  # Create dataframe for current drug route
  drug_route <- data.frame(
    lon = c(port_lng, receiver_lng),
    lat = c(port_lat, receiver_lat)
  )
  
  # Add drug route dataframe to the list
  drug_routes[[i]] <- drug_route
}

for (i in 1:length(drug_routes)) {
  m <- m %>%
    addGeodesicPolylines(data = drug_routes[[i]], lng = ~lon, lat = ~lat, color = "black", weight = 2, steps = 10,
                         opacity = 0.5)
}

?addGeodesicPolylines

m
