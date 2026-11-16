# 1. Load required packages
library(fpp3)
library(fpp2)
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