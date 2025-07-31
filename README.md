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
```
├── models/                # Trained models, weights, or model configuration files
├── outputs/               # Forecast results
└── README.md              # Project overview and documentation
```
</pre>


---

## Libraries and Dependencies

A list of all libraries and packages used to process the data and train your model:

- `tidyverse`
- `lubridate`
- `scikit-learn`
- `tsibble`
- `feasts`
- `fable`
- `fabletools`
- `readr`
- `ggplot2`
- `mgcv`
- `Metrics`
- `randomForest`

---

## Data and Variables

### Datasets and variables used
- Dengue.csv

- Climate.csv  

### Data preprocessing
- For the random forest model, the climate variables used (med temperature, med precipitation and med rel humidity) were lagged by 8 weeks, and states were clustered into 4 clusters based on climate similarity to account for geographic effects. Dates were transformed into a seasonality parameter by taking the cosine and sine of the week.  Predicted case values were smoothed over a 16 week window (aligned center). 

### Variable selection
Predictors were chosen based on optimal model performances by trial-and-error. For random forest model, climatic predictors included med temperature, med precipitation and med rel humidity.

---

## Model Training

### Training details
- Describe model architecture or algorithms used.
    
- Specify any cross-validation or time series split methods.

- For the random forest model, we ran a random forest regresion of 1000 iterations with the state as a fixed effect. 

### Hyperparameter optimization
- Detail any grid search, Bayesian optimization, or manual tuning used.

### Training code
- Point to the specific script or notebook.

---

## Data Usage Restriction

- Forecasts from EW 41 (year N) to EW 40 (year N+1) are based **only** on data up to EW 25 (year N).
  
- Describe how this was enforced in preprocessing or model training.

---

## Predictive Uncertainty

- How are your prediction intervals computed?
