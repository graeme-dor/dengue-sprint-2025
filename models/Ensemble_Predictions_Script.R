## Ensemble predictions
# Predictions with the least abs error 

# Read in results from different models - T1 predictions
RF_tra1 <- fread("target3_rf_predictions_with_intervals_with_errors_smoothed.csv")
names(RF_tra1)

LSTM_tra1 <- fread("LSTM_predictions_Train3_JP.csv")
names(LSTM_tra1)

## Join the two datasets 
# Step 2: Join on Date and State
merged <- full_join(RF_tra1, LSTM_tra1, by = c("date", "uf"), suffix = c("_1", "_2"))

# Step 3: Choose prediction with smaller error, handling NAs
final_df <- merged %>%
  mutate(
    best_model = case_when(
      is.na(absolute_error) & !is.na(abs_error) ~ "model2",
      is.na(abs_error) & !is.na(absolute_error) ~ "model1",
      is.na(abs_error) & is.na(absolute_error) ~ NA_character_,
      absolute_error <= abs_error ~ "model1",
      absolute_error > abs_error ~ "model2"
    ),
    Best_Prediction = case_when(
      best_model == "model1" ~ median,
      best_model == "model2" ~ pred,
      TRUE ~ NA_real_
    ),
    Best_Error = case_when(
      best_model == "model1" ~ absolute_error,
      best_model == "model2" ~ abs_error,
      TRUE ~ NA_real_
    ),
    CI_Low = case_when(
      best_model == "model1" ~ lower_95,
      best_model == "model2" ~ ci_lower,
      TRUE ~ NA_real_
    ),
    CI_High = case_when(
      best_model == "model1" ~ upper_95,
      best_model == "model2" ~ ci_upper,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(date, uf,admin1_cases, Best_Prediction, Best_Error, CI_Low, CI_High, best_model)


write_csv(final_df,"Ens_Predictions_Target3.csv")

## Overall MAE and RMSE
overall_metrics <- final_df %>%
  summarise(
    MAE = mean(abs(Best_Prediction - admin1_cases), na.rm = TRUE),
    RMSE = sqrt(mean((Best_Prediction - admin1_cases)^2, na.rm = TRUE))
  )

overall_metrics

# Per-state MAE and RMSE
statewise_metrics <- final_df[, .(
  MAE = mean(abs(Best_Prediction - admin1_cases), na.rm = TRUE),
  RMSE = sqrt(mean((Best_Prediction - admin1_cases)^2, na.rm = TRUE))
), by = uf]

statewise_metrics <- as.data.frame(statewise_metrics)

write_csv(statewise_metrics,"Ens_Predictions_Target3_MAE_prstate.csv")


## Plotting 
plot_obs_pred <- ggplot(final_df, aes(x = date)) +
  geom_line(aes(y = admin1_cases, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Best_Prediction, color = "Predicted"), size = 1.2) +
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
plot_obs_pred



