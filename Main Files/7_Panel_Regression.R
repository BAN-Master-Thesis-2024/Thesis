# NHH - 7: Multiple Regression with Panel-Data # 

# Master Thesis # 


# Library
library(tidyverse)
library(parallel)
library(stats)
library(dplyr)
library(plm)
library(geosphere)
library(viridis)
library(hrbrthemes)
library(stargazer)

library(randomForest)
library(lme4)

library(caret)
library(kableExtra)
library(groupdata2)

library(MuMIn)

library(xgboost)

# Loading data 
entities_descriptive <- read.csv("CSV-files/Panel-Data/entities_descriptive2.csv") # Entity Panel-data
companies_cl <- read.csv("CSV-files/Maps_Geocoding/companies_cl2.csv") # All companies

companies_cl <- companies_cl[-c(1)]

locations <- read.csv("CSV-files/Maps_Geocoding/Locations_Google_cl.csv") # Coordinates

havens_hotspots <- read.csv("CSV-files/Maps_Geocoding/havens_hotspots.csv") # Hotspots

# External Shocks: U.S aid, Cocaine Crops and Cocaine Seizure
coca_crops <- read.csv("CSV-files/Coca_Data/coca_crops.csv")
coca_seizure <- read.csv("CSV-files/Coca_Data/coca_seizure.csv")
us_aid <- read.csv("CSV-files/Coca_Data/col_aid.csv")

# ------------------------- # 
#  Possible Shock-Variables # 
# ------------------------- # 

# Calculate lead and lag (1-3)
coca_crops$lag <- dplyr::lag(coca_crops$cultivation_hectares)
coca_crops$lag_2 <- dplyr::lag(coca_crops$cultivation_hectares, 2)
coca_crops$lag_3 <- dplyr::lag(coca_crops$cultivation_hectares, 3)

coca_crops$lead <- dplyr::lead(coca_crops$cultivation_hectares)
coca_crops$lead_2 <- dplyr::lead(coca_crops$cultivation_hectares, 2)
coca_crops$lead_3 <- dplyr::lead(coca_crops$cultivation_hectares, 2)

us_aid$lag <- dplyr::lag(us_aid$aid_millions)
us_aid$lag_2 <- dplyr::lag(us_aid$aid_millions, 2)
us_aid$lag_3 <- dplyr::lag(us_aid$aid_millions, 3)

us_aid$lead <- dplyr::lead(us_aid$aid_millions)
us_aid$lead_2 <- dplyr::lead(us_aid$aid_millions, 2)
us_aid$lead_3 <- dplyr::lead(us_aid$aid_millions, 3)

coca_seizure$lag <- dplyr::lag(coca_seizure$tot_tonne)
coca_seizure$lag_2 <- dplyr::lag(coca_seizure$tot_tonne, 2)
coca_seizure$lag_3 <- dplyr::lag(coca_seizure$tot_tonne, 3)

coca_seizure$lead <- dplyr::lead(coca_seizure$tot_tonne)
coca_seizure$lead_2 <- dplyr::lead(coca_seizure$tot_tonne, 2)
coca_seizure$lead_3 <- dplyr::lead(coca_seizure$tot_tonne, 3)

# Calculate absolute %-changes 
coca_crops <- coca_crops %>% 
  mutate(crops_change_1yr = (cultivation_hectares/lag-1)*100,
         crops_change_2yr = (cultivation_hectares/lag_2-1)*100,
         crops_change_3yr = (cultivation_hectares/lag_3-1)*100)

us_aid <- us_aid %>% 
  mutate(aid_change_1yr = (aid_millions/lag-1)*100,
         aid_change_2yr = (aid_millions/lag_2-1)*100,
         aid_change_3yr = (aid_millions/lag_3-1)*100)

coca_seizure <- coca_seizure %>% 
  mutate(seizures_change_1yr = (coca_seizure/lag-1)*100,
         seizures_change_2yr = (coca_seizure/lag_2-1)*100,
         seizures_change_3yr = (coca_seizure/lag_3-1)*100)
  
# --------------------------- # 
# Distance to nearest hotspot #
# --------------------------- # 

companies_cl$nearest_hotspot <- NULL
companies_cl$hotspot_dist <- NULL

for (i in 1:nrow(companies_cl)) {
  
  # Extract linked coordinate of entity
  lon_i <- companies_cl$lon[i]
  lat_i <- companies_cl$lat[i]
  
  # Find distances to hotspots
  
  hotspot_distance <- NULL # Distance
  hotspot <- c() # Hotspot 
  
  for (j in 1:nrow(havens_hotspots)) {
    
    # Extract coordinates 
    lon_j <- havens_hotspots$x[j]
    lat_j <- havens_hotspots$y[j]
    
    hotspot_j <- havens_hotspots$hotspot[j]
    
    # Calculate haversine-distance
    distance <- distm(c(lon_i, lat_i), c(lon_j, lat_j), fun = distHaversine)
    
    hotspot_distance[j] <- distance # Save
    hotspot[j] <- hotspot_j # Save
    
  }
  
  # Save shortest distance
  min_dist <- min(hotspot_distance)
  index <- which.min(hotspot_distance)
  
  companies_cl$hotspot_dist[i] <- min_dist
  
  if (!is_empty(index)) {
    
    companies_cl$nearest_hotspot[i] <- hotspot[index]
  }

}

companies_cl$hotspot_dist <- round(companies_cl$hotspot_dist, 4)

# ------------------------ # 
# Adding Coordinates etc   #
# ------------------------ # 

# Initiate columns
entities_descriptive$lat <- NA
entities_descriptive$lon <- NA
entities_descriptive$loctype <- NA
entities_descriptive$type_adr <- NA
entities_descriptive$nearest_hotspot <- NA
entities_descriptive$hotspot_dist <- NA


# Loop
for (i in 1:nrow(entities_descriptive)) {
  
  # Filter for entity
  address_info <- companies_cl %>% 
    filter(node_id == entities_descriptive$entity_id[i]) %>% 
    dplyr::select(lat, lon, loctype, type_adr, hotspot_dist, nearest_hotspot)
  
  # Skip if no info
  if (nrow(address_info) < 1) {
    next
  }
  
  # Add data
  entities_descriptive$lat[i] <- address_info$lat[1]
  entities_descriptive$lon[i] <- address_info$lon[1]
  entities_descriptive$loctype[i] <- address_info$loctype[1]
  entities_descriptive$type_adr[i] <- address_info$type_adr[1]
  
  # Hotspot-distance
  entities_descriptive$nearest_hotspot[i] <- address_info$nearest_hotspot[1]
  entities_descriptive$hotspot_dist[i] <- address_info$hotspot_dist[1]

  next
  
}

write.csv(entities_descriptive, "CSV-files/entities_descriptive_cl2.csv")

# Read file (for future use)
entities_descriptive <- read.csv("CSV-files/entities_descriptive_cl2.csv")


# ------------ # 
# Longevity #
# ------------ # 
entities_descriptive <- entities_descriptive %>% 
  group_by(entity_id) %>% 
  mutate(longevity = row_number())

# ---------------- # 
# Preparing Data   #
# ---------------- # 

# Filter observations = 0
reg_data <- entities_descriptive %>% 
  filter(!nr_officers == 0) %>% 
  filter(year > 1989 & year < 2016)

# Loop for adding data to each year

cl <- makeCluster(6)

for (i in 1:nrow(reg_data)) {
  
  # Retrieve year and entity_id
  year_i <- reg_data$year[i]
  entity_i <- reg_data$entity_id[i]

  # ------- # 
  # Extract # 
  # ------- # 
  
  # Crops
  crops <- coca_crops %>% 
    filter(year == year_i)
  
  # U.S. Aid
  aid <- us_aid %>% 
    filter(Fiscal.Year == year_i)
  
  # Cocaine-seizure
  coca_seize <- coca_seizure %>% 
    filter(Year == year_i)
  
  # Search-Term
  term <- companies_cl %>% 
    filter(node_id == reg_data$entity_id[i])
  
  # Address
  address_i <- companies_cl %>% 
    filter(node_id == entity_i) %>% 
    dplyr::select(address)
  
  if (!is.null(address_i)) { 
    
    # Coordinate
    coordinates_i <- locations %>% 
      filter(address_original == address_i[1, ])
    
    # Save
    reg_data$lat[i] <- coordinates_i$lat[1]
    reg_data$lon[i] <- coordinates_i$lon[1]
    reg_data$type[i] <- coordinates_i$type[1]
    reg_data$loctype[i] <- coordinates_i$loctype[1]
    
  }
  
  else {
    
    # NAs
    reg_data$lat[i] <- NA
    reg_data$lon[i] <- NA
  }
  
  # ------- # 
  # Save # 
  # ------- # 
  
  # Cocaine Crops 
  reg_data$coca_crops[i] <- crops$cultivation_hectares[1]
  reg_data$coca_crops_lag[i] <- crops$lag[1]
  reg_data$coca_crops_lead[i] <- crops$lead[1] 
  reg_data$crops_change_1yr[i] <- crops$crops_change_1yr[1]
  reg_data$crops_change_2yr[i] <- crops$crops_change_2yr[1]
  reg_data$crops_change_3yr[i] <- crops$crops_change_3yr[1]
  
  # U.S. Aid
  reg_data$us_aid[i] <- aid$aid_millions[1]
  reg_data$us_aid_lag[i] <- aid$lag[1]
  reg_data$us_aid_lead[i] <- aid$lead[1]
  reg_data$aid_change_1yr[i] <- aid$aid_change_1yr[1]
  reg_data$aid_change_2yr[i] <- aid$aid_change_2yr[1]
  reg_data$aid_change_3yr[i] <- aid$aid_change_3yr[1]
  
  # Cocaine Seized
  reg_data$coca_seized[i] <- coca_seize$tot_tonne[1]
  reg_data$coca_seized_lag[i] <- coca_seize$lag[1]
  reg_data$coca_seized_lead[i] <- coca_seize$lead[1]
  
  # Other variables
  reg_data$term[i] <- term$term[1]
  reg_data$origin[i] <- term$origin[1]


}

stopCluster(cl)


# --------------------- # 
# Create new Variables  #
# --------------------- # 

reg_data$coca_crops_ha_1000 <- reg_data$coca_crops/1000

# --------------------- # 
# 1 - Time Dimension Variables # 
# --------------------- # 

# Plan Colombia Launch
reg_data$post_pc <- ifelse(reg_data$year > 2000, 1, 0) # Post launch of Plan Colombia 

# Time-periods
reg_data <- reg_data %>%
  mutate(period = case_when(
    year %in% c(1995:2000) ~ " pre_pc ",
    year %in% c(2001:2005) ~ " post_pc "
  ))

# --------------------- # 
# 2 - Geospatial variables 
# --------------------- # 

# Preparation of data 
reg_data$in_hotspot <- ifelse(reg_data$hotspot_dist < 500, 1, 0)

reg_data$hotspot_dist_km <- reg_data$hotspot_dist/1000

reg_data$jurisdiction <- ifelse(reg_data$jurisdiction %in% c("PAN", "PMA", "PA"), "PMA", 
                         reg_data$jurisdiction)

reg_data$jurisdiction <- ifelse(reg_data$jurisdiction %in% c("VG", "VGB"), "BVI", 
                                reg_data$jurisdiction)

reg_data$jurisdiction <- ifelse(reg_data$jurisdiction %in% c("KY", "CAYMN"), "CAYMN", 
                                reg_data$jurisdiction)


# --------------------- # 
# 2 - Shock Dummies
# --------------------- # 

# Dummies for major positive production shocks
reg_data$cultivation_shock <- ifelse(reg_data$crops_change_1yr > 50, 1, 0)
reg_data$cultivation_shock_2 <- ifelse(reg_data$crops_change_2yr > 50, 1, 0)

# Dummies for major negative political shocks
reg_data$aid_shock <- ifelse(reg_data$aid_change_1yr > 50, 1, 0)
reg_data$aid_shock2 <- ifelse(reg_data$aid_change_2yr > 50, 1, 0)

# Save data 
write.csv(reg_data, "CSV-files/reg_data.csv")

reg_data <- read.csv("CSV-files/reg_data.csv")


# ----------------------------- # 
# ----- Panel Regression  ---- #
# ---------------------------- # 

# Cleaning data for regression # 
count_limit <- reg_data %>% 
 # filter(nr_officers > 0 & nr_officers < 40) %>% 
  group_by(entity_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 2) # At least two observations 
  

#  PLM
reg_data_cl <- reg_data %>% 
  filter(entity_id %in% count_limit$entity_id) %>% 
  group_by(entity_id) %>% 
  filter(nr_officers > 0 & nr_officers < 50) %>%  # Remove outliers and NA-observations
  filter(type_adr %in% c("street_address", "street_address_manual", "locality", 
                         "premise", "neighborhood", "establishment", "subpremise"))   # Remove entities with inaccurate coordinates

write.csv(reg_data_cl, "CSV-files/reg_data_cl.csv") # Save data 

read.csv("CSV-files/reg_data_cl.csv")


# Standard effects 
mod_1 <- plm(log(nr_officers) ~ in_hotspot + coca_crops_ha_1000 + us_aid,
             data = reg_data_cl,
             index = c("entity_id", "year"),
             model = "random",
             effect = "time")

summary(mod_1)

# Shocks & Time-Dependent Trend 
mod_2 <- plm(log(nr_officers) ~ in_hotspot + cultivation_shock + aid_shock + post_pc,
             data = reg_data_cl,
             index = c("entity_id", "year"),
             model = "random",
             effect = "time")

summary(mod_2)


# Cultivation Shock Hotspots
mod_3 <- plm(log(nr_officers) ~ in_hotspot:cultivation_shock + aid_shock,
             data = reg_data_cl,
             index = c("entity_id", "year"),
             model = "random",
             effect = "time")

summary(mod_3)


# Aid Shock Hotspots 
mod_4 <- plm(log(nr_officers) ~ in_hotspot:aid_shock + cultivation_shock,
             data = reg_data_cl,
             index = c("entity_id", "year"),
             model = "random",
             effect = "time")

summary(mod_4)

# Seasonal Effects Hotspot
mod_5 <- plm(log(nr_officers) ~ in_hotspot:post_pc + cultivation_shock + aid_shock,
             data = reg_data_cl,
             index = c("entity_id", "year"),
             model = "random",
             effect = "time")

summary(mod_5)


# Individual Effects 
mod_6 <- plm(log(nr_officers) ~ in_hotspot + cultivation_shock + post_pc + aid_shock,
             data = reg_data_cl,
             index = c("entity_id", "year"),
             model = "random",
             effect = "individual")

summary(mod_6)


# Individual Effects Hotspots
mod_7 <- plm(log(nr_officers) ~ in_hotspot:cultivation_shock + post_pc + aid_shock,
             data = reg_data_cl,
             index = c("entity_id", "year"),
             model = "random",
             effect = "individual")

summary(mod_7)

# Individual Effects Hotspots
mod_8 <- plm(log(nr_officers) ~ in_hotspot:aid_shock + post_pc + cultivation_shock,
             data = reg_data_cl,
             index = c("entity_id", "year"),
             model = "random",
             effect = "individual")

summary(mod_8)


# ----------- #  
# Output # 

# Models
models <- c("Base Models",
            "Base Models",
            "Hotspots",
            "Hotspots",
            "Hotspots")

# Output
output_1 <- stargazer(list(mod_1, mod_2, mod_3, mod_4, mod_5),
                  title = "Panel Regression ",
                  type = "html",
                  out = "FE_time.html",
                  column.labels = models) # Saving as html.file for output 


output_2 <- stargazer(list(mod_6, mod_7, mod_8),
                      title = "Panel Regression ",
                      type = "html",
                      out = "FE_individual.html",
                      column.labels = models) # Saving as html.file for output 

# ------------------------- # 
# ----- K-fold CV --------- #
# ------------------------- # 

set.seed(1) # For reproducilibity

# Ungroup data 
cv_data <- reg_data_cl %>% 
  ungroup()

cv_data$entity_id_factor <- as.factor(cv_data$entity_id)
cv_data$log_nr_officers <- log(cv_data$nr_officers)

# --------------- # 
# CV-function # 
# --------------- # 

# Function for RMSE 
rmse_log <- function(pred, target) {
  sqrt(mean((pred - target)^2))
}

# Function for cross validating
cross_validate <- function(cv_data, k, model_type) {
  
  # cv_data - full subset panel data (no holdout)
  # k - nr of folds
  # model_type - type of model (plm, ols, lmer or rf)
  
  
  set.seed(123) # For reproduction

  # Divide train-set into k folds 
  training_set <-  fold(cv_data, k = k, id_col = "entity_id_factor")
  
  training_sset <- training_set %>% 
    arrange(.folds)

  
  # Vectors for tracking performance 
  performance_RMSE <- c() # RMSE
  performance_R2 <- c() # Standard R-squared
  performance_cor <- c() # Pearson correlation
  
  # Loop for folds 
  for (fold in 1:k) {
    
    # Training fold for iteration, k -1 folds 
    training_fold <- training_set[training_set$.folds != fold,]
    
    # Test set for iteration, 1 fold 
    testing_fold <- training_set[training_set$.folds == fold,]


    # Panel-model
    if (model_type == "plm") { 
      
      # Train plm-model
      plm_model <- plm(formula = log(nr_officers) ~ in_hotspot + cultivation_shock + aid_shock + post_pc,
                       data = training_fold,
                       index = c("entity_id", "year"),
                       model = "random",
                       effect = "time")
    
      
      
      # Extract RMSE 
      predicted <- predict(plm_model, testing_fold, allow.new.levels = TRUE) # Predictions 
      
      RMSE <- rmse_log(predicted, testing_fold[["log_nr_officers"]])
      
      # Extract R2
      sum <- summary(plm_model)
      
      R2 <- sum$r.squared
      
      # Pearson correlation
      pearson_cor <- cor(predicted, testing_fold[["log_nr_officers"]], method = "pearson" )

      # Save data 
      performance_RMSE[fold] <- RMSE # RMSE 
      performance_R2[fold] <- R2[[1]] # R2
      performance_cor[fold] <- pearson_cor

    }
    
    
    # Random-Forest 
    if (model_type == "rf") {
      
      # Train model 
      rf_mod <- randomForest(log(nr_officers) ~ in_hotspot + cultivation_shock + aid_shock + post_pc,
                             data = training_fold,
                             ntree = 500) # 500 trees 
      
      # Extract RMSE
      rf_pred <- predict(rf_mod, testing_fold, allow.new.levels = TRUE)
      
      RMSE <- rmse_log(rf_pred, testing_fold[["log_nr_officers"]])
      
      # Extract R2
      R2 <- mean(rf_mod$rsq) # Mean of R2 for all 500 trees 
      
      # Correlation
      pearson_cor <- cor(rf_pred, testing_fold[["log_nr_officers"]], method = "pearson")
      
      
      # Save data 
      performance_RMSE[fold] <- RMSE # RMSE 
      performance_R2[fold] <- R2 # R2
      performance_cor[fold] <- pearson_cor
      
      
    }
    # Linear Mixed Effect model 
    if (model_type == "lmer") {
      
      # Train model 
      lmer_mod <- lmer(log(nr_officers) ~ in_hotspot + cultivation_shock + aid_shock + post_pc + 
                         (1 | entity_id) + (1 | year), data = training_fold)
      
      # Extract RMSE
      lmer_pred <- predict(lmer_mod, testing_fold, allow.new.levels = TRUE)
      
      RMSE <- rmse_log(lmer_pred, testing_fold[["log_nr_officers"]])
      
      # Extract R2 
      # Utilizing MuMIn-library to calculate marginal R2
      R2 <- r.squaredGLMM(lmer_mod, condVar = TRUE) 
      
      # Correlation
      pearson_cor <- cor(lmer_pred, testing_fold[["log_nr_officers"]], method = "pearson")
      
      # Save data 
      performance_RMSE[fold] <- RMSE # RMSE 
      performance_R2[fold] <- R2[[1]] # R2 for fixed-effects 
      performance_cor[fold] <- pearson_cor # Pearson correlation
      
    }
    
    # Standard OLS (Pooling PLM)
    if (model_type == "ols") {
      
      # Train model 
      plm_model <- plm(formula = log(nr_officers) ~ in_hotspot + cultivation_shock + aid_shock + post_pc,
                       data = training_fold,
                       index = c("entity_id", "year"),
                       model = "pooling")
      
      # Extract RMSE
      ols_pred <- predict(plm_model, testing_fold, allow.new.levels = TRUE)
      
      RMSE <- rmse_log(ols_pred, testing_fold[["log_nr_officers"]])
      
      
      # Extract R2
      sum <- summary(plm_model)
      
      R2 <- sum$r.squared
      
      # Correlation
      pearson_cor <- cor(ols_pred, testing_fold[["log_nr_officers"]], method = "pearson")
    
      # Save data 
      performance_RMSE[fold] <- RMSE # RMSE 
      performance_R2[fold] <- R2[[1]] # Standard R2
      performance_cor[fold] <- pearson_cor # Pearson correlation

    }

 
  }
  
  # Return mean RMSE, R2 and pearson-corr
  results <- data.frame(RMSE = mean(performance_RMSE),
                        R2 = mean(performance_R2),
                        corr = mean(performance_cor))
  
  return(results)
  
  
}


# --------------------------------------------------------- # 
# Running separate loops for each model-type 
# --------------------------------------------------------- # 
# Note: Running one loop for each o avoid a big "messy" 
# function and easier debugging 

# ------ # 
# PLM 
# ------ # 

# Loop for assessing k (2-10) for best plm-model
performances_plm <- data.frame(model_type = c(),
                           folds = integer(),
                           RMSE = numeric(),
                           r_squared = numeric(),
                           corr = numeric())

for (i in 2:15) {
  
  # Apply function
  result <- cross_validate(cv_data = cv_data, k = i,  model_type = "plm")
  
  # Extract results 
  RMSE <- result[[1]]
  R2 <- result[[2]]
  pearson <- result[[3]]
  
  # Save in a df
  result <- data.frame(model_type = "plm",
                       folds = i,
                       rmse = RMSE,
                       r_squared = R2,
                       corr = pearson)
  
  # Append to initial df
  performances_plm <- rbind(performances_plm, result)

}


# ------ # 
# OLS
# ------ # 
# Loop for assessing k (2-10) for best ols-model
performances_ols <- data.frame(model_type = c(),
                               folds = integer(),
                               RMSE = numeric(),
                               r_squared = numeric(),
                               corr = numeric())

for (i in 2:15) {
  
  # Apply function
  result <- cross_validate(cv_data = cv_data, k = i,  model_type = "ols")
  
  # Extract results 
  RMSE <- result[[1]]
  R2 <- result[[2]]
  pearson <- result[[3]]
  
  
  # Save in a df
  result <- data.frame(model_type = "ols",
                       folds = i,
                       rmse = RMSE,
                       r_squared = R2,
                       corr = pearson)
  
  # Append to initial df
  performances_ols <- rbind(performances_ols, result)
  
}

# ------ # 
# LMER
# ------ # 

# Loop for assessing k (2-10) for best lmer-model
performances_lmer <- data.frame(model_type = c(),
                               folds = integer(),
                               RMSE = numeric(),
                               r_squared = numeric(),
                               corr = numeric())

for (i in 2:15) {
  
  # Apply function
  result <- cross_validate(cv_data = cv_data, k = i,  model_type = "lmer")
  
  # Extract results 
  RMSE <- result[[1]]
  R2 <- result[[2]]
  pearson <- result[[3]]
  
  # Save in a df
  result <- data.frame(model_type = "lmer",
                       folds = i,
                       rmse = RMSE,
                       r_squared = R2,
                       corr = pearson)
  
  # Append to initial df
  performances_lmer <- rbind(performances_lmer, result)
  
}

# ------ # 
# RF
# ------ # 
# Loop for assessing k (2-10) for best lmer-model
performances_rf <- data.frame(model_type = c(),
                                folds = integer(),
                                RMSE = numeric(),
                                r_squared = numeric(),
                              corr = numeric())


cl <- makeCluster(6)

for (i in 2:15) {
  
  # Apply function
  result <- cross_validate(cv_data = cv_data, k = i,  model_type = "rf")
  
  # Extract results 
  RMSE <- result[[1]]
  R2 <- result[[2]]
  pearson <- result[[3]]
  
  # Save in a df
  result <- data.frame(model_type = "rf",
                       folds = i,
                       rmse = RMSE,
                       r_squared = R2,
                       corr = pearson)
  
  # Append to initial df
  performances_rf <- rbind(performances_rf, result)
  
}



performances_full <- rbind(performances_lmer, performances_ols, performances_plm, performances_rf)

write.csv(perfomances_full, "CSV-files/performance_metrics.csv")



# ------ # 
# Prepare for plots # 
# ------ # 
performances_full$full_name <- ifelse(performances_full$model_type == "lmer", "Linear Mixed Effects", 0)
performances_full$full_name <- ifelse(performances_full$model_type == "rf", "Random Forest", performances_full$full_name)
performances_full$full_name <- ifelse(performances_full$model_type == "ols", "Pooled OLS", performances_full$full_name)
performances_full$full_name <- ifelse(performances_full$model_type == "plm", "Random Effects", performances_full$full_name)


performances_full <- performances_full %>% 
  filter(!model_type == "rf")

# RMSE plot 
ggplot() + 
  geom_line(data = performances_full, aes(x = folds, y = exp(rmse), color = full_name)) +
  geom_point(data = performances_full, aes(x = folds, y = exp(rmse), color = full_name, shape = full_name)) + 
  labs(y = "RMSE", x = "K-Fold", title = "K-Fold Validation for RMSE") + 
  ggthemes::theme_fivethirtyeight() + 
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))


# R2 plot 
ggplot() + 
  geom_line(data = performances_full, aes(x = folds, y = r_squared, color = full_name)) + 
  geom_point(data = performances_full, aes(x = folds, y = r_squared, color = full_name, shape = full_name)) + 
  labs(y = "R-Squared", x = "K-Fold", title = "K-Fold Validation for R-Squared") + 
  ggthemes::theme_fivethirtyeight() + 
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))


# Pearson Corr plot 
ggplot() + 
  geom_line(data = performances_full, aes(x = folds, y = corr, color = full_name)) + 
  geom_point(data = performances_full, aes(x = folds, y = corr, color = full_name, shape = full_name)) + 
  labs(y = "Pearson Correlation", x = "K-Fold", title = "K-Fold Validation for Pearson Correlation") + 
  ggthemes::theme_fivethirtyeight() + 
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))


# ------------------------- # 
# ----- Box-Plots --------- #
# ------------------------- # 

# Boxplot 1 - Nr of officers by jurisdiction 
ggplot(data = reg_data_cl, aes(x = factor(jurisdiction), y = log(nr_officers))) +
  geom_boxplot(aes(alpha = 2, fill = factor(jurisdiction))) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
  geom_jitter(alpha = 0.3, size = 0.2) +
  theme_ipsum() +
  theme(legend.position = "none")

# Boxplot 2 - Nr of officers by distance to nearest hotspot
ggplot(data = reg_data_cl, aes(x = factor(in_hotspot), y = nr_officers)) +
  geom_boxplot(aes(alpha = 2, fill = factor(in_hotspot))) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
  geom_jitter(alpha = 0.3, size = 0.2) +
  theme_ipsum() +
  theme(legend.position = "none")

# Boxplot 3 - Nr of officers by company_type
ggplot(data = reg_data_cl, aes(x = factor(post_pc), y = nr_officers)) +
  geom_boxplot(aes(alpha = 2, fill = factor(post_pc))) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
  geom_jitter(alpha = 0.3, size = 0.2) +
  theme_ipsum() +
  theme(legend.position = "none")

