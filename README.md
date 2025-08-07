# 2025 Sprint: 2nd Infodengue-Mosqlimate Dengue Challenge (IMDC)

## Team and Contributors

**CERI Forecasting Club**

*Team members: Jenicca Poongavanan, Monika Moir, Gaspary Mwanyika, Graeme Dor, Houriiyah Tegally* 

<sub>
Organisation: Centre for Epidemic Response and Innovation (CERI), School of Data Science and Computational Thinking, Stellenbosch University, Stellenbosch, South Africa
</sub>

---

## Repository Structure

A brief description of the contents and purpose of each folder and file in the repository:
<pre>
├── models/                # Trained models
├── outputs/               # Forecast results
└── README.md              # Project overview and documentation
</pre>


---

## Libraries and Dependencies

A list of all libraries and packages used to process the data and train the models:

- `tidyverse`
- `lubridate`
- `tsibble`
- `feasts`
- `fable`
- `fabletools`
- `readr`
- `ggplot2`
- `mgcv`
- `Metrics`
- `randomForest`
- `scikit-learn`
- `keras`
- `tensorflow`
- `reticulate`

---

## Data and Variables

### Datasets and Variables Used

- `dengue.csv` – Weekly dengue case counts at the municipal level  
- `climate.csv` – Historical climate variables (temperature, precipitation, relative humidity)  
- `forecasted_climate.csv` – Climate forecasts used for EW26–EW40  

### Data Preprocessing

- **Random Forest**: Climate predictors (median temperature, precipitation, and relative humidity) were lagged by 8 weeks and joined with the dengue case data. States were grouped into 4 climate similarity clusters using hierarchical clustering based on average climate features. Weekly seasonality was encoded using sine and cosine transformations of the epidemiological week. Final predictions were smoothed using a centered rolling mean over a 16-week window.  
- **LSTM**: Models were trained using sequences of weekly dengue case counts (with optional inclusion of lagged climate covariates), structured into input windows. Unlike RF, LSTM did not use clustering or spatial groupings. Time steps were set to 52 weeks.  
- **Ensemble**: Predictions from RF and LSTM were evaluated per state, and the model with the lowest error (based on RMSE) was selected as the final ensemble prediction for that state.

### Variable Selection

- **Random Forest**: Climatic variables included median temperature, median precipitation, and median relative humidity. Variables were selected based on iterative testing and error minimization.  
- **LSTM**: Primarily trained on dengue case histories with and without climate covariates, using combinations that yielded the lowest validation error per state.

---

## Model Training

### Training Details

- **Random Forest**  
  - Implemented using `randomForest` in R  
  - Regression task using weekly dengue cases as the outcome  
  - 1000 trees per model; `state` included as a fixed covariate  
  - Trained separately for each climate cluster, then applied to corresponding states  
  - Cross-validation was based on historical forecasting windows (rolling origin)  

- **LSTM**  
  - Implemented using `keras` with `tensorflow` backend in R  
  - Standard LSTM architecture: 1 hidden LSTM layer with 64 units, followed by a dense output layer  
  - Trained with Adam optimizer, mean squared error loss, and early stopping  
  - Validation based on walk-forward strategy over test periods  

- **Ensemble**  
  - Final prediction = best-performing model (LSTM or RF) per state, selected based on lowest error on validation period  
  - No additional meta-model or weighted average was used  

### Hyperparameter Optimization

- **Random Forest**  
  - Number of trees fixed at 1000  
  - No grid search required; performance evaluated empirically  

- **LSTM**  
  - Manual tuning of number of LSTM units (32–128), batch size (16–64), and input window size (fixed at 52)  
  - Early stopping used to prevent overfitting  

---

## Training Code

- All model training scripts can be found under the `models/` directory.  
- Separate R scripts are provided for Random Forest and LSTM training.

---

## Data Usage Restriction

- Forecasts from **EW 41 (Year N)** to **EW 40 (Year N+1)** were generated using only data available **up to EW 25 (Year N)**.  
- For **climate**, values from EW26 to EW40 were filled using `forecasted_climate.csv` (as provided).  
- This restriction was enforced by:  
  - Filtering the training datasets to exclude any case or climate data after EW25  
  - Aligning lagged covariates to ensure that EW41–EW40 predictions never used future information  

---

## Predictive Uncertainty

- **Prediction intervals** were constructed empirically:  
  - For **Random Forest**, we computed the standard deviation across the predictions of all trees (bootstrap sampling) to estimate uncertainty bounds.  
  - For **LSTM**, prediction intervals were approximated using residual bootstrapping across multiple model runs.  
  - For the **Ensemble**, we applied the uncertainty intervals from the selected model per state (either RF or LSTM).
