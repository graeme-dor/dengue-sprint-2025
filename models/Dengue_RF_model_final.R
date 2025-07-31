## Modelling

# Load required libraries
library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(readr)
library(ggplot2)
library(mgcv)
library(Metrics)
library(randomForest)



## Load the dengue dataset
epi_data <- read.csv(file = "data_sprint_2025/dengue.csv")

# Load the location dataset
location_data<-read.csv(file="data_sprint_2025/map_regional_health.csv")
biome_data<-read.csv(file="data_sprint_2025/environ_vars.csv")

location_data<-merge(location_data,biome_data)
location_data_uf <- location_data %>% select(c("uf","biome"))  %>% unique()

##Load climate dataset
climate_data<-read.csv(file="data_sprint_2025/climate.csv")

climate_data<-merge(climate_data,location_data)
climate_data<-merge(climate_data,location_data_uf)

# Convert date to Date format and logical training flags 
epi_data <- epi_data %>%
  mutate(date = as.Date(date),
         train_1 = as.logical(train_1),
         target_1 = as.logical(target_1),
         train_2 = as.logical(train_2),
         target_2 = as.logical(target_2),
         train_3 = as.logical(train_3),
         target_3 = as.logical(target_3)) # (i.e. boolean operator - True/False)

climate_data <- climate_data %>%
  mutate(date = as.Date(date)) # (i.e. boolean operator - True/False)


# Filter and aggregate training data for Validation 1 at the national level
national_train <- epi_data %>%
  filter(train_1 == TRUE) %>%
  group_by(date) %>%
  summarise(cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date) %>%
  ungroup()

original_national_train<-national_train

# Aggregate climate data for at the national level
national_climate <- climate_data %>%
  group_by(date) %>%
  summarise(temp_min = mean(temp_min, na.rm = TRUE),
            temp_max = mean(temp_max, na.rm = TRUE),
            temp_med = mean(temp_med, na.rm = TRUE),
            precip_min = mean(precip_min, na.rm = TRUE),
            precip_max = mean(precip_max, na.rm = TRUE),
            precip_med = mean(precip_med, na.rm = TRUE),
            rel_humid_min = mean(rel_humid_min, na.rm = TRUE),
            rel_humid_max = mean(rel_humid_max, na.rm = TRUE),
            rel_humid_med = mean(rel_humid_med, na.rm = TRUE),
            thermal_range = mean(thermal_range, na.rm = TRUE),
            rainy_days = mean(rainy_days, na.rm = TRUE)) %>%
  arrange(date) %>%
  ungroup()
original_national_climate<-national_climate


# Aggregate climate data for at admin 1 level
admin1_climate <- climate_data %>%
  group_by(date,uf,biome) %>%
  summarise(temp_min = mean(temp_min, na.rm = TRUE),
            temp_max = mean(temp_max, na.rm = TRUE),
            temp_med = mean(temp_med, na.rm = TRUE),
            precip_min = mean(precip_min, na.rm = TRUE),
            precip_max = mean(precip_max, na.rm = TRUE),
            precip_med = mean(precip_med, na.rm = TRUE),
            rel_humid_min = mean(rel_humid_min, na.rm = TRUE),
            rel_humid_max = mean(rel_humid_max, na.rm = TRUE),
            rel_humid_med = mean(rel_humid_med, na.rm = TRUE),
            thermal_range = mean(thermal_range, na.rm = TRUE),
            rainy_days = mean(rainy_days, na.rm = TRUE)) %>%
  arrange(date) %>%
  ungroup()

original_admin1_climate<-admin1_climate



# Filter and aggregate training data for Validation 1 at the admin2 level
admin1_train <- epi_data %>%
  filter(train_1 == TRUE) %>%
  group_by(date,uf) %>%
  summarise(admin1_cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date,uf) %>%
  ungroup()

original_admin1_train<- admin1_train

admin2_train <- epi_data %>%
  filter(train_1 == TRUE) %>%
  group_by(date,geocode) %>%
  summarise(admin2_cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date,geocode) %>%
  ungroup()

original_admin2_train<- admin2_train


original_admin1_train<- admin1_train

admin1_train3 <- epi_data %>%
  filter(train_3 == TRUE) %>%
  group_by(date,uf) %>%
  summarise(admin1_cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date,uf) %>%
  ungroup()

original_admin1_train3<-admin1_train3


admin1_train2 <- epi_data %>%
  filter(train_2 == TRUE) %>%
  group_by(date,uf) %>%
  summarise(admin1_cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date,uf) %>%
  ungroup()
original_admin1_train2<-admin1_train2

# Filter and aggregate target data for Target 1 at the national level
national_target <- epi_data %>%
  filter(target_1 == TRUE) %>%
  group_by(date) %>%
  summarise(cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date) %>%
  ungroup()

original_national_target<-national_target


# Filter and aggregate target data for Target 1,2,3
admin1_target <- epi_data %>%
  filter(target_1 == TRUE) %>%
  group_by(date,uf) %>%
  summarise(admin1_cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date,uf) %>%
  ungroup()

admin1_target3 <- epi_data %>%
  filter(target_3 == TRUE) %>%
  group_by(date,uf) %>%
  summarise(admin1_cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date,uf) %>%
  ungroup()


admin1_target2 <- epi_data %>%
  filter(target_2 == TRUE) %>%
  group_by(date,uf) %>%
  summarise(admin1_cases = sum(casos, na.rm = TRUE)) %>%
  arrange(date,uf) %>%
  ungroup()

original_admin1_target<-admin1_target
original_admin1_target3<-admin1_target3
original_admin1_target2<-admin1_target2

national_train$week_of_year <- as.numeric(format(national_train$date, "%U")) + 1  # Weeks 1–53
national_target$week_of_year <- as.numeric(format(national_target$date, "%U")) + 1  # Weeks 1–53



## geographic indicators

library(sf)
library(dplyr)

# Load Brazil Admin1 shapefile
shp <- st_read("gadm41_BRA_shp/gadm41_BRA_1.shp")

# Calculate centroids
shp <- shp %>%
  mutate(centroid = st_centroid(geometry),
         latitude = st_coordinates(centroid)[,2],
         longitude = st_coordinates(centroid)[,1],
         uf = sapply(strsplit(HASC_1, "\\."), `[`, 2))

# Merge with your time series data (assuming you have a `state_code` or `region` column)
original_admin1_train3 <- original_admin1_train3 %>%
  left_join(st_drop_geometry(shp) %>% select(uf, latitude, longitude), by = "uf")



### Random forest model - admin1 with covariates
admin1_train2<-original_admin1_train2
admin1_target2<-original_admin1_target2



admin1_climate<-original_admin1_climate


# Rolling average and lagged versions
climate_vars <- c("temp_med", "precip_med", "rel_humid_med")
#climate_vars <- c("TP")

for (var in climate_vars) {
  admin1_climate <- admin1_climate %>%
    group_by(uf) %>%
    mutate(!!paste0(var, "_lag_1") := lag(.data[[var]], 1),
           !!paste0(var, "_lag_2") := lag(.data[[var]], 2),
           !!paste0(var, "_lag_4") := lag(.data[[var]], 4),
           !!paste0(var, "_lag_8") := lag(.data[[var]], 8),
           !!paste0(var, "_lag_16") := lag(.data[[var]], 16),
           !!paste0(var, "_roll7") := lag(zoo::rollmean(.data[[var]], 7, fill = NA, align = "right"),2))
}

admin1_train2<-merge(admin1_train2,admin1_climate, by=c("uf","date"))
admin1_target2<-merge(admin1_target2, admin1_climate, by=c("uf","date"))



# Compute average climate per region
climate_summary <- admin1_train2 %>%
  group_by(uf) %>%
  summarize(temp_mean = mean(temp_med_lag_8, na.rm = TRUE),
            rain_mean = mean(precip_med_lag_8, na.rm = TRUE),
            humidity_mean = mean(rel_humid_med_lag_8, na.rm = TRUE))

climate_summary <- admin1_target2 %>%
  group_by(uf) %>%
  summarize(temp_mean = mean(temp_med_lag_8, na.rm = TRUE),
            rain_mean = mean(precip_med_lag_8, na.rm = TRUE),
            humidity_mean = mean(rel_humid_med_lag_8, na.rm = TRUE))

# K-means clustering (e.g., 4 clusters)
set.seed(123)
climate_summary$cluster_id <- kmeans(climate_summary[,2:3], centers = 4)$cluster

# Merge cluster ID back to main data
admin1_train2 <- admin1_train2 %>%
  left_join(climate_summary %>% select(uf, cluster_id), by = "uf")

admin1_target2 <- admin1_target2 %>%
  left_join(climate_summary %>% select(uf, cluster_id), by = "uf")


admin1_train2 <- admin1_train2 %>%
  group_by(uf) %>%
  
  mutate(
    week = isoweek(date),
    month = month(date),
    sin_week = sin(2 * pi * week / 52),
    cos_week = cos(2 * pi * week / 52)
  )

admin1_train2 <- admin1_train2 %>%
  group_by(uf,week) %>%
  #mutate(avg_cases_by_week = mean(admin1_cases, na.rm = TRUE)) %>%
  ungroup()



admin1_target2 <- admin1_target2 %>%
  group_by(uf) %>%
  
  mutate(
    week = isoweek(date),
    month = month(date),
    sin_week = sin(2 * pi * week / 52),
    cos_week = cos(2 * pi * week / 52)
  )

admin1_target2 <- admin1_target2 %>%
  group_by(uf,week) %>%
  #mutate(avg_cases_by_week = mean(admin1_cases, na.rm = TRUE)) %>%
  ungroup()


admin1_train2 <- admin1_train2 %>% drop_na()

library(quantregForest) #to run RF with confidence intervals


# Prepare training data
predictors <- c("uf", "week", "month", "sin_week", "cos_week",
  "temp_med_lag_8", "precip_med_lag_8", "rel_humid_med_lag_8", "cluster_id")

qrf_model <- quantregForest(
  x = admin1_train2[, predictors],   # predictors is a vector of column names
  y = admin1_train2$admin1_cases,
  ntree = 1000
)

# Define quantiles for required intervals
quantiles <- c(0.025, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.975)

# Predict quantiles
qrf_preds <- predict(qrf_model, newdata = admin1_target2[, predictors], what = quantiles)

# Add intervals to test_data
admin1_target2$lower_50 <- qrf_preds[, "quantile= 0.25"]
admin1_target2$upper_50 <- qrf_preds[, "quantile= 0.75"]

admin1_target2$lower_80 <- qrf_preds[, "quantile= 0.1"]
admin1_target2$upper_80 <- qrf_preds[, "quantile= 0.9"]

admin1_target2$lower_90 <- qrf_preds[, "quantile= 0.05"]
admin1_target2$upper_90 <- qrf_preds[, "quantile= 0.95"]

admin1_target2$lower_95 <- qrf_preds[, "quantile= 0.025"]
admin1_target2$upper_95 <- qrf_preds[, "quantile= 0.975"]

# Median prediction
admin1_target2$median <- qrf_preds[, "quantile= 0.5"]


# smoothing over predictions and intervals

admin1_target2_predictions<-admin1_target2 %>% select("uf","date","median","lower_50","upper_50","lower_80","upper_80","lower_90","upper_90","lower_95","upper_95")

admin1_target2_errors<-admin1_target2 %>% select("uf","date","admin1_cases","median","lower_50","upper_50","lower_80","upper_80","lower_90","upper_90","lower_95","upper_95")
admin1_target2_errors<-admin1_target2 %>% select("uf","date","admin1_cases","median","lower_50","upper_50","lower_80","upper_80","lower_90","upper_90","lower_95","upper_95")
admin1_target2_errors <- admin1_target2_errors %>%
  mutate(
    absolute_error = abs(admin1_cases - median),
    squared_error = (admin1_cases - median)^2
  )


admin1_target2_predictions_smoothed<-admin1_target2_predictions
admin1_target2_predictions_smoothed$median <- rollmean(admin1_target2_predictions_smoothed$median, k = 16, fill = NA, align = "center")
admin1_target2_predictions_smoothed$lower_50 <- rollmean(admin1_target2_predictions_smoothed$lower_50, k = 16, fill = NA, align = "center")
admin1_target2_predictions_smoothed$upper_50 <- rollmean(admin1_target2_predictions_smoothed$upper_50, k = 16, fill = NA, align = "center")
admin1_target2_predictions_smoothed$lower_80 <- rollmean(admin1_target2_predictions_smoothed$lower_80, k = 16, fill = NA, align = "center")
admin1_target2_predictions_smoothed$upper_80 <- rollmean(admin1_target2_predictions_smoothed$upper_80, k = 16, fill = NA, align = "center")
admin1_target2_predictions_smoothed$lower_90 <- rollmean(admin1_target2_predictions_smoothed$lower_90, k = 16, fill = NA, align = "center")
admin1_target2_predictions_smoothed$upper_90 <- rollmean(admin1_target2_predictions_smoothed$upper_90, k = 16, fill = NA, align = "center")
admin1_target2_predictions_smoothed$lower_95 <- rollmean(admin1_target2_predictions_smoothed$lower_95, k = 16, fill = NA, align = "center")
admin1_target2_predictions_smoothed$upper_95 <- rollmean(admin1_target2_predictions_smoothed$upper_95, k = 16, fill = NA, align = "center")


admin1_target2_errors_smoothed<-admin1_target2_errors
admin1_target2_errors_smoothed$median <- rollmean(admin1_target2_errors_smoothed$median, k = 16, fill = NA, align = "center")
admin1_target2_errors_smoothed$lower_50 <- rollmean(admin1_target2_errors_smoothed$lower_50, k = 16, fill = NA, align = "center")
admin1_target2_errors_smoothed$upper_50 <- rollmean(admin1_target2_errors_smoothed$upper_50, k = 16, fill = NA, align = "center")
admin1_target2_errors_smoothed$lower_80 <- rollmean(admin1_target2_errors_smoothed$lower_80, k = 16, fill = NA, align = "center")
admin1_target2_errors_smoothed$upper_80 <- rollmean(admin1_target2_errors_smoothed$upper_80, k = 16, fill = NA, align = "center")
admin1_target2_errors_smoothed$lower_90 <- rollmean(admin1_target2_errors_smoothed$lower_90, k = 16, fill = NA, align = "center")
admin1_target2_errors_smoothed$upper_90 <- rollmean(admin1_target2_errors_smoothed$upper_90, k = 16, fill = NA, align = "center")
admin1_target2_errors_smoothed$lower_95 <- rollmean(admin1_target2_errors_smoothed$lower_95, k = 16, fill = NA, align = "center")
admin1_target2_errors_smoothed$upper_95 <- rollmean(admin1_target2_errors_smoothed$upper_95, k = 16, fill = NA, align = "center")


admin1_target2_errors_smoothed <- admin1_target2_errors_smoothed %>%
  mutate(
    absolute_error = abs(admin1_cases - median),
    squared_error = (admin1_cases - median)^2
  )

write.csv(admin1_target2_predictions, "target2_rf_predictions_with_intervals.csv", row.names = FALSE)
write.csv(admin1_target2_errors, "target2_rf_predictions_with_intervals_with_erros.csv", row.names = FALSE)

write.csv(admin1_target2_predictions, "target2_rf_predictions_with_intervals_smoothed.csv", row.names = FALSE)
write.csv(admin1_target2_errors_smoothed, "target2_rf_predictions_with_intervals_with_errors_smoothed.csv", row.names = FALSE)


admin1_target2_errors_smoothed <- admin1_target2_errors_smoothed %>% drop_na()
#visualise results
mae_val <- mae(admin1_target2_errors_smoothed$admin1_cases, admin1_target2_errors_smoothed$median)
rmse_val <- rmse(admin1_target2_errors_smoothed$admin1_cases, admin1_target2_errors_smoothed$median)
r2_val <- cor(admin1_target2_errors_smoothed$admin1_cases, admin1_target2_errors_smoothed$median)^2

cat("Test MAE:", round(mae_val, 2), "\n")
cat("Test RMSE:", round(rmse_val, 2), "\n")
cat("Test R²:", round(r2_val, 3), "\n")

admin1_target2_errors_smoothed <- admin1_target2_errors_smoothed %>% group_by(uf) %>%
  mutate(mae=mae(admin1_cases,median), rmse=rmse(admin1_cases,median)) %>%
  mutate(
    facet_label = paste0(
      uf, "\n",
      "MAE: ", round(mae, 1), " | ",
      "RMSE: ", round(rmse, 1), "\n"
    ))



prediction_results<-ggplot(admin1_target2_errors_smoothed, aes(x = date)) +
  geom_line(data=admin1_target2_errors_smoothed,aes(x=date,y = admin1_cases), color = "black") +
  geom_ribbon(data=admin1_target2_errors_smoothed,aes(x=date,ymin = lower_50,ymax=upper_50), fill = "blue",alpha=0.3) +
  geom_ribbon(data=admin1_target2_errors_smoothed,aes(x=date,ymin = lower_80,ymax=upper_80), fill = "blue",alpha=0.2) +
  geom_ribbon(data=admin1_target2_errors_smoothed,aes(x=date,ymin = lower_90,ymax=upper_90), fill = "blue",alpha=0.1) +
  geom_ribbon(data=admin1_target2_errors_smoothed,aes(x=date,ymin = lower_95,ymax=upper_95), fill = "blue",alpha=0.05) +
  geom_line(data=admin1_target2_errors_smoothed,aes(x=date,y = median), color = "blue") +
  facet_wrap(~ facet_label, scales = "free_y") +
  
  labs(title = "Random Forest Forecast vs Actual Cases") +
  theme_minimal()+
  labs(title = "Forecast vs. Actual Cases",
       y = "Cases", x = "Date",
       subtitle = sprintf("MAE = %.1f, RMSE = %.1f",
                          mae_val, rmse_val)) 
prediction_results
