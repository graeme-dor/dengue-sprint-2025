# ------------------------------
# LSTM - Model Fitting 
# ------------------------------
# Load libraries
library(data.table)
library(dplyr)
library(abind)
library(keras)
library(lubridate)
library(ggplot2)
library(tidyverse)

# ------------------------------
# STEP 1: Aggregate by UF + Date & Add Seasonality
# ------------------------------


## Read in EPI data 
epi_data <- fread("brazil_all_data 2.csv")

## Preparing the data 
# Aggregating to administrative level 1 
# Change train and target region as needed
epi_uf <- epi_data %>%
  mutate(week = week(date)) %>%
  group_by(uf, date) %>%
  dplyr::summarise(
    casos = sum(casos),
    across(temp_min:thermal_range, mean, na.rm = TRUE),
    train_1 = any(train_1),
    target_1 = any(target_1),
    week = first(week),  # gets week number for this date
    .groups = "drop"
  ) %>%
  group_by(uf) %>%
  mutate(
    week_centered = week - mean(week, na.rm = TRUE),
    sin_week = sin(2 * pi * week_centered / 52),
    cos_week = cos(2 * pi * week_centered / 52)
  ) %>%
  ungroup() %>%
  arrange(uf, date)

# ------------------------------
# STEP 2: Scale Covariates + Target
# ------------------------------
covariates <- epi_uf[, c(5,14,21,22)]  # includes seasonal sin/cos
covariates_scaled <- scale(covariates)
casos_scaled <- scale(epi_uf$casos)

epi_scaled <- cbind(epi_uf[, 1:2], casos_scaled, covariates_scaled, epi_uf[, c("train_1", "target_1")])
names(epi_scaled)[3] <- "casos_scaled"

# ------------------------------
# STEP 3: Create Sequences by UF
# ------------------------------
n_timesteps <- 52
X_list <- list()
Y_list <- list()
train_flags <- c()
test_flags <- c()

for (uf_i in unique(epi_scaled$uf)) {
  df <- epi_scaled %>%
    filter(uf == uf_i) %>%
    arrange(date)
  
  covariate_names <- colnames(covariates_scaled)
  scaled_df <- df[, c("casos_scaled", covariate_names)]
  
  for (i in seq(n_timesteps + 1, nrow(df))) {
    X_list[[length(X_list) + 1]] <- as.matrix(scaled_df[(i - n_timesteps):(i - 1), -1])
    Y_list[[length(Y_list) + 1]] <- scaled_df[i, 1]
    
    train_flags <- c(train_flags, df$train_1[i])
    test_flags <- c(test_flags, df$target_1[i])
  }
}

X_array <- abind::abind(X_list, along = 1)
dim(X_array) <- c(length(Y_list), n_timesteps, ncol(covariates_scaled))
Y_vector <- unlist(Y_list)

# ------------------------------
# STEP 4: Split Train/Test
# ------------------------------
train_idx <- which(train_flags)
test_idx <- which(test_flags)

x_train <- X_array[train_idx, , ]
y_train <- Y_vector[train_idx]

x_test <- X_array[test_idx, , ]
y_test <- Y_vector[test_idx]

# ------------------------------
# STEP 5: Build and Train LSTM
# ------------------------------
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(n_timesteps, ncol(covariates_scaled))) %>%
  layer_dense(units = 1) # "relu to ensure positive predictions otw 0 

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

model %>% fit(
  x_train, y_train,
  epochs = 50,
  batch_size = 32,
  validation_data = list(x_test, y_test)
)

# ------------------------------
# STEP 6: Evaluate on Unscaled Data
# ------------------------------
pred_scaled <- predict(model, x_test)

center <- attr(casos_scaled, "scaled:center")
scale_ <- attr(casos_scaled, "scaled:scale")

pred <- as.numeric(pred_scaled) * scale_ + center
actual <- as.numeric(y_test) * scale_ + center

mae <- mean(abs(pred - actual))
rmse <- sqrt(mean((pred - actual)^2))

cat("MAE:", round(mae, 2), "\n")
cat("RMSE:", round(rmse, 2), "\n")

# ------------------------------
# STEP 7: Plotting for each UF
# ------------------------------
valid_rows <- epi_scaled %>%
  group_by(uf) %>%
  arrange(date) %>%
  slice((n_timesteps + 1):n()) %>%
  ungroup()

results_df <- valid_rows[test_idx, ] %>%
  mutate(
    actual = actual,
    pred = pred
  )

ggplot(results_df, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = pred, color = "Predicted")) +
  facet_wrap(~ uf, scales = "free_y") +
  labs(title = "Dengue Predictions by UF", y = "Cases", x = "Date") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    title = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 14, face = "bold")
  )

#### Saving it into a csv with obs / pred/ errors/ CI 
results_df <- results_df %>%
  mutate(
    abs_error = abs(actual - pred),
    sq_error = (actual - pred)^2
  )

# Residual standard deviation (for CI)
resid_sd <- sd(results_df$actual - results_df$pred, na.rm = TRUE)

# 95% CI using standard normal approximation
z <- 1.96  # for 95%
results_df <- results_df %>%
  mutate(
    ci_lower = pred - z * resid_sd,
    ci_upper = pred + z * resid_sd
  )
write.csv(results_df, "LSTM_predictions_Train3_JP.csv", row.names = FALSE)

####-----------------------------------------------------------------## 
# LSTM - per state - each their own variables 
####-----------------------------------------------------------------## 

# The list of the top five variables is from 

top_5_for_LSTM <- shap_top5_per_state[,c(6,1)]
names(top_5_for_LSTM)[1] <- "uf"

top5_vars_per_state <- top_5_for_LSTM

# Final LSTM modeling per state using top 5 SHAP covariates
library(dplyr)
library(keras)
library(abind)

results <- list()

for (state in unique(top5_vars_per_state$uf)) {
  message("Processing: ", state)
  
  # Top 5 features for this state
  top_vars <- top5_vars_per_state %>% filter(uf == state) %>% pull(feature)
  
  # Subset and scale
  df <- epi_uf %>% filter(uf == state)
  df <- df %>% dplyr::select(date, casos, train_1, target_1, all_of(top_vars)) %>% arrange(date)
  
  # Remove NAs
  df <- na.omit(df)
  
  # Scale
  scaled_covariates <- scale(df[, top_vars])
  casos_scaled <- scale(df$casos)
  center <- attr(casos_scaled, "scaled:center")
  scale_ <- attr(casos_scaled, "scaled:scale")
  
  df <- cbind(date = df$date, casos_scaled = casos_scaled, scaled_covariates, df[, c("train_1", "target_1")])
  
  # Create sequences
  n_timesteps <- 52
  X <- list()
  Y <- c()
  train_flags <- c()
  test_flags <- c()
  
  for (i in seq(n_timesteps + 1, nrow(df))) {
    X[[length(X) + 1]] <- as.matrix(df[(i - n_timesteps):(i - 1), top_vars])
    Y <- c(Y, df$casos_scaled[i])
    train_flags <- c(train_flags, df$train_1[i])
    test_flags <- c(test_flags, df$target_1[i])
  }
  
  X_array <- abind::abind(X, along = 1)
  dim(X_array) <- c(length(Y), n_timesteps, length(top_vars))
  
  x_train <- X_array[which(train_flags), , ]
  y_train <- Y[which(train_flags)]
  x_test  <- X_array[which(test_flags), , ]
  y_test  <- Y[which(test_flags)]
  
  # LSTM
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(n_timesteps, length(top_vars))) %>%
    layer_dense(units = 1)
  
  model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')
  
  model %>% fit(x_train, y_train, epochs = 50, batch_size = 32, verbose = 0)
  
  pred_scaled <- predict(model, x_test)
  pred <- as.numeric(pred_scaled) * scale_ + center
  actual <- y_test * scale_ + center
  
  mae <- mean(abs(pred - actual))
  rmse <- sqrt(mean((pred - actual)^2))
  
  results[[state]] <- list(mae = mae, rmse = rmse, actual = actual, pred = pred, date = df$date[(n_timesteps + 1):nrow(df)][which(test_flags)])
}

# Collect evaluation
performance <- do.call(rbind, lapply(names(results), function(state) {
  data.frame(uf = state,
             MAE = results[[state]]$mae,
             RMSE = results[[state]]$rmse)
}))

print(performance)

# Create results_df from the list if not done yet
results_df <- purrr::map_dfr(names(results), function(state) {
  tibble(
    uf = state,
    date = results[[state]]$date,
    actual = results[[state]]$actual,
    predicted = results[[state]]$pred,
    mae = results[[state]]$mae,
    rmse = results[[state]]$rmse
  )
})

# Keep only one row per state for annotation placement
metrics_df <- results_df %>%
  group_by(uf) %>%
  filter(date == max(date)) %>%
  slice(1) %>%
  ungroup()

# Plot
plot_obs_pred <- ggplot(results_df, aes(x = date)) +
  geom_line(aes(y = actual, color = "Observed"), size = 1.2) +
  geom_line(aes(y = predicted, color = "Predicted"), size = 1.2) +
  geom_text(
    data = metrics_df,
    aes(x = date, y = Inf, label = paste0("MAE: ", round(mae, 1), "\nRMSE: ", round(rmse, 1))),
    hjust = 1, vjust = 1.1,
    inherit.aes = FALSE,
    size = 3.5
  ) +
  facet_wrap(~ uf, scales = "free_y") +
  labs(
    title = "Observed vs Predicted Dengue Cases by State (LSTM)",
    x = "Date",
    y = "Cases",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(plot_obs_pred)


## Overall MAE and RMSE for 
overall_metrics <- results_df %>%
  summarise(
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  )

print(overall_metrics)


