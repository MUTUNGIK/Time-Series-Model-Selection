# 1. Load required packages
library(fpp3)
library(tidyverse)
library(tidyr)
library(forecast)
library(e1071)
library(tseries)
library(tsfeatures)
library(pracma)
library(rpart)

# 2. Load feature extraction functions
source("feature_extraction.R")

# 3. Load model recommendation function
source("recommend_model.R")


set.seed(123)

n <- 120
t <- 1:n
h <- 12

seasonal_ts <- ts(
  100 + 0.2*t + 15*sin(2*pi*t/12) + rnorm(n, sd = 5),
  frequency = 12
)

nonseasonal_ts <- ts(
  100 + as.numeric(arima.sim(list(ar = 0.6), n = n, sd = 5)),
  frequency = 12
)

# Extract features
seasonal_features <- extract_features(seasonal_ts, h = h, freq = 12)
nonseasonal_features <- extract_features(nonseasonal_ts, h = h, freq = 1)

# Obtain recommendations
recommend_model(seasonal_features, seasonal = TRUE)
recommend_model(nonseasonal_features, seasonal = FALSE)