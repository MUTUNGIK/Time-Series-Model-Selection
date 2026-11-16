# Feature-Based Forecasting Model Recommendation

This repository provides an R implementation of a decision-tree-based approach for recommending forecasting models using time series characteristics.

The software recommends one of four forecasting models:

- ARIMA
- Chronos
- LSTM
- N-BEATS

Separate classification trees are provided for seasonal and nonseasonal time series. The trees were developed using forecasting performance and time series features from the M4 competition dataset.

## Repository Contents

| File | Description |
|---|---|
| `feature_extraction.R` | Functions for extracting time series characteristics |
| `recommend_model.R` | Function for generating forecasting model recommendations |
| `example.R` | Reproducible examples using simulated seasonal and nonseasonal series |
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
source("example.R")
```

Alternatively, load the functions and apply them to your own time series:

```r
source("feature_extraction.R")
source("recommend_model.R")

features <- extract_features(my_series, h = 12)

recommend_model(features, seasonal = TRUE)
```

Here, `my_series` is a numeric time series, `h` is the forecasting horizon, and `seasonal` specifies whether the seasonal or nonseasonal decision tree should be used.

## Example Results

Using the simulated series provided in `example.R`, the fitted trees produced the following recommendations:

| Simulated series | Recommended model |
|---|---|
| Seasonal | Chronos |
| Nonseasonal | LSTM |

These recommendations illustrate the operation of the software and do not establish that the recommended models are necessarily the most accurate for the simulated series.

## Methodological Note

The decision trees are intended to provide interpretable, feature-based forecasting model recommendations. Class weighting was applied during tree fitting to improve representation of the four candidate models.

The recommendations are exploratory and should not be interpreted as guarantees of superior forecasting accuracy.
