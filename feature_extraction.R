

# -------------------------------------------------------
# Required packages
# -------------------------------------------------------

library(fpp3)

library(tidyverse)
library(tidyr)
library(forecast)
library(e1071)
library(tseries)
library(tsfeatures)
library(purrr)

scale_ts <- function(x) {
  x <- stats::na.omit(x)
  if (length(x) < 2) return(as.numeric(x))
  as.numeric(scale(x))
}

stl_value <- function(x, key) {
  z <- tryCatch(tsfeatures::stl_features(stats::ts(x, frequency = 12)),
                error = function(e) NULL)
  if (is.null(z) || !key %in% names(z)) return(NA_real_)
  as.numeric(z[[key]])
}

acf_value <- function(x, key) {
  z <- tryCatch(tsfeatures::acf_features(stats::ts(x, frequency = 12)),
                error = function(e) NULL)
  if (is.null(z) || !key %in% names(z)) return(NA_real_)
  as.numeric(z[[key]])
}

# Original monthly feature definitions (frequency = 12)
feature_kurtosis <- function(x) {
  z <- tryCatch(kurtosis(stats::ts(x, frequency = 12)),
                error = function(e) NA_real_)
  as.numeric(z[[1]])
}
feature_skewness <- function(x) {
  z <- tryCatch(skewness(stats::ts(x, frequency = 12)),
                error = function(e) NA_real_)
  as.numeric(z[[1]])
}
feature_nonlinearity <- function(x) {
  z <- tryCatch(nonlinearity(stats::ts(x, frequency = 12)),
                error = function(e) NA_real_)
  as.numeric(z[[1]])
}

feature_hurst_empirical <- function(series_scaled, max_diff = 11) {
  x <- as.numeric(na.omit(series_scaled))
  if (length(x) < 5) return(NA_real_)
  
  diff_order <- 0
  repeat {
    if (length(x) < 5 || diff_order > max_diff) return(NA_real_)
    
    p_val <- tryCatch(adf.test(x)$p.value,
                      error = function(e) NA_real_)
    
    if (!is.na(p_val) && p_val < 0.05) {
      hurst <- tryCatch({
        invisible(capture.output(pracma::hurstexp(x)))
        pracma::hurstexp(x)
      }, error = function(e) NULL)
      
      return(if (!is.null(hurst)) hurst$Hal else NA_real_)
    }
    
    x <- diff(x)
    diff_order <- diff_order + 1
  }
}

feature_trend_strength <- function(x) stl_value(x, "trend")
feature_spikiness <- function(x) stl_value(x, "spike")
feature_linearity <- function(x) stl_value(x, "linearity")
feature_curvature <- function(x) stl_value(x, "curvature")
feature_residual_acf_1 <- function(x) stl_value(x, "e_acf1")
feature_seasonal_strength <- function(x) stl_value(x, "seasonal_strength")
feature_acf1 <- function(x) acf_value(x, "acf1")
feature_acf10 <- function(x) acf_value(x, "acf10")
feature_seas_acf1 <- function(x) acf_value(x, "seas_acf1")

feature_lambda_guerrero <- function(x) {
  if (length(x) < 24) return(NA_real_)
  tryCatch(as.numeric(forecast::BoxCox.lambda(stats::ts(x, frequency = 12),
                                             method = "guerrero")),
           error = function(e) NA_real_)
}
feature_flat_spots <- function(x) {
  z <- tsfeatures::flat_spots(stats::ts(x, frequency = 12))
  as.numeric(z[["flat_spots"]])
}
feature_stability <- function(x) {
  as.numeric(tsfeatures::stability(stats::ts(x, frequency = 12)))
}
feature_unitroot_pp <- function(x) {
  as.numeric(tsfeatures::unitroot_pp(stats::ts(x, frequency = 12)))
}
feature_unitroot_kpss <- function(x) {
  as.numeric(tsfeatures::unitroot_kpss(stats::ts(x, frequency = 12)))
}
feature_ets_parameter <- function(x, parameter) {
  fit <- forecast::ets(stats::ts(x, frequency = 12), model = "AAA")
  as.numeric(fit$par[[parameter]])
}

# Extract all predictors retained after the original exclusion list.
# h is the requested forecast horizon; series_length/Horizon = length_horizon.
extract_features <- function(x, h = 12) {
  stopifnot(is.numeric(x), length(h) == 1, is.finite(h), h > 0)
  x <- scale_ts(as.numeric(x))
  if (length(x) < 24) stop("At least 24 non-missing observations are required for monthly STL features.")
  

  monthly <- stats::ts(x, frequency = 12)
  data.frame(
    length_horizon = length(x) / h,
    kurtosis = feature_kurtosis(x),
    skewness = feature_skewness(x),
    non_linearity = feature_nonlinearity(x),
    trend_strength = feature_trend_strength(x),
    spikiness = feature_spikiness(x),
    linearity = feature_linearity(x),
    curvature = feature_curvature(x),
    residual_acf_1 = feature_residual_acf_1(x),
    lambda = feature_lambda_guerrero(x),
    seasonal_strength = feature_seasonal_strength(x),
    entropy = as.numeric(tsfeatures::entropy(monthly)),
    acf10 = feature_acf10(x),
    acf1 = feature_acf1(x),
    seas_acf1 = feature_seas_acf1(x),
    flat_spots = feature_flat_spots(x),
    alpha = feature_ets_parameter(x, "alpha"),
    hurst_exp = feature_hurst_empirical(x),
    beta = feature_ets_parameter(x, "beta"),
    gamma = feature_ets_parameter(x, "gamma"),
    crossing_points = as.numeric(tsfeatures::crossing_points(monthly)),
    outlier_prop = as.numeric(get("outlierinclude_mdrmd", asNamespace("tsfeatures"))(monthly)),
    walker_prop = as.numeric(get("walker_propcross", asNamespace("tsfeatures"))(monthly)),
    taures = as.numeric(get("localsimple_taures", asNamespace("tsfeatures"))(monthly)),
    stability = feature_stability(x),
    unitroot_pp = feature_unitroot_pp(x),
    unitroot_kpss = feature_unitroot_kpss(x),
    check.names = FALSE
  )
}
