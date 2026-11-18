# Feature-Based Forecasting Model Recommendation

This repository provides an R implementation of a decision-tree-based approach that recommends forecasting models based on time-series characteristics. The trees were developed using forecasting performance and time series features extracted from series in the M4 competition dataset. The dataset is divided into seasonal and nonseasonal datasets; thus, two separate classification trees are developed.

The implementation recommends one of four forecasting models:

- ARIMA
- Chronos
- LSTM
- N-BEATS

## Repository Contents

| File | Description |
|---|---|
| `feature_extraction.R` | Functions for extracting time series characteristics |
| `recommend_model.R` | Function for generating forecasting model recommendations |
| `Example.R` | Reproducible examples using simulated seasonal and nonseasonal series |
| `tree_seasonal.rds` | Fitted classification tree for seasonal series |
| `tree_nonseasonal.rds` | Fitted classification tree for nonseasonal series |

## Requirements

The software is implemented in R. The following packages are used:

```r
install.packages(c(
  "fpp3", "tidyverse", "tidyr",
  "forecast", "e1071", "tseries",
  "tsfeatures", "purrr", "pracma", "rpart"
))
```

## Usage

Download or clone this repository and set the R working directory to the folder containing the files.

To run the complete demonstration:

```r
source("Example.R")
```

Alternatively, load the functions and apply them to your own time series:

```r
source("feature_extraction.R")
source("recommend_model.R")

# Extract features for seasonal and non-seasonal series
seasonal_features <- extract_features(seasonal_ts, h = 12, freq = 12)
nonseasonal_features <- extract_features(nonseasonal_ts, h = 12, freq = 1)

# Recommend forecasting models
recommend_model(seasonal_features, seasonal = TRUE)
recommend_model(nonseasonal_features, seasonal = FALSE)
```

Here, `my_series` is a numeric time series, `h` is the forecasting horizon, and `seasonal` specifies whether the seasonal or nonseasonal decision tree should be used.

## Example Results

Using the simulated series provided in `Example.R`, the fitted trees produced the following recommendations:

| Simulated series | Recommended model |
|---|---|
| Seasonal | Chronos |
| Nonseasonal | LSTM |

These recommendations illustrate the operation of the software and do not establish that the recommended models are necessarily the most accurate for the simulated series.

## Note

The decision trees are intended to provide interpretable, feature-based forecasting model recommendations. Class weighting was applied during tree fitting to improve representation of the four candidate models.

The recommendations are exploratory and should not be interpreted as guarantees of superior forecasting accuracy.
