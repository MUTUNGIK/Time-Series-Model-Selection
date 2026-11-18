

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
library(pracma)

# -------------------------------------------------------
# Helper functions
# -------------------------------------------------------

scale_ts <- function(x) {
  x <- stats::na.omit(x)
  
  if (length(x) < 2) {
    return(as.numeric(x))
  }
  
  z <- as.numeric(scale(x))
  
  # Avoid NaN when the series is constant
  if (all(is.na(z))) {
    return(rep(0, length(x)))
  }
  
  z
}

# Return a single numeric value, or NA if unavailable
safe_numeric <- function(expr) {
  tryCatch({
    z <- suppressWarnings(expr)
    z <- as.numeric(z)
    
    if (length(z) == 0 || !is.finite(z[1])) {
      return(NA_real_)
    }
    
    z[1]
  }, error = function(e) NA_real_)
}

# -------------------------------------------------------
# STL and autocorrelation features
# -------------------------------------------------------

stl_value <- function(x, key, freq) {
  
  series_ts <- stats::ts(x, frequency = freq)
  
  z <- tryCatch(
    tsfeatures::stl_features(series_ts),
    error = function(e) NULL
  )
  
  if (is.null(z) || !key %in% names(z)) {
    return(NA_real_)
  }
  
  safe_numeric(z[[key]])
}

acf_value <- function(x, key, freq) {
  
  series_ts <- stats::ts(x, frequency = freq)
  
  z <- tryCatch(
    tsfeatures::acf_features(series_ts),
    error = function(e) NULL
  )
  
  if (is.null(z) || !key %in% names(z)) {
    return(NA_real_)
  }
  
  safe_numeric(z[[key]])
}

# -------------------------------------------------------
# Distributional features
# -------------------------------------------------------

feature_kurtosis <- function(x, freq) {
  safe_numeric(
    e1071::kurtosis(stats::ts(x, frequency = freq))
  )
}

feature_skewness <- function(x, freq) {
  safe_numeric(
    e1071::skewness(stats::ts(x, frequency = freq))
  )
}

feature_nonlinearity <- function(x, freq) {
  safe_numeric(
    tsfeatures::nonlinearity(
      stats::ts(x, frequency = freq)
    )
  )
}

# -------------------------------------------------------
# Empirical Hurst exponent
# -------------------------------------------------------

feature_hurst_empirical <- function(
    series_scaled,
    max_diff = 11
) {
  
  x <- as.numeric(stats::na.omit(series_scaled))
  
  if (length(x) < 5) {
    return(NA_real_)
  }
  
  diff_order <- 0
  
  repeat {
    
    if (length(x) < 5 || diff_order > max_diff) {
      return(NA_real_)
    }
    
    p_val <- tryCatch(
      suppressWarnings(tseries::adf.test(x)$p.value),
      error = function(e) NA_real_
    )
    
    if (!is.na(p_val) && p_val < 0.05) {
      
      hurst <- tryCatch({
        invisible(capture.output(
          result <- pracma::hurstexp(x)
        ))
        result
      }, error = function(e) NULL)
      
      if (is.null(hurst)) {
        return(NA_real_)
      }
      
      return(safe_numeric(hurst$Hal))
    }
    
    x <- diff(x)
    diff_order <- diff_order + 1
  }
}

# -------------------------------------------------------
# STL-based features
# -------------------------------------------------------

feature_trend_strength <- function(x, freq) {
  stl_value(x, "trend", freq)
}

feature_spikiness <- function(x, freq) {
  stl_value(x, "spike", freq)
}

feature_linearity <- function(x, freq) {
  stl_value(x, "linearity", freq)
}

feature_curvature <- function(x, freq) {
  stl_value(x, "curvature", freq)
}

feature_residual_acf_1 <- function(x, freq) {
  stl_value(x, "e_acf1", freq)
}

feature_seasonal_strength <- function(x, freq) {
  
  if (freq <= 1) {
    return(NA_real_)
  }
  
  stl_value(x, "seasonal_strength", freq)
}

# -------------------------------------------------------
# Autocorrelation features
# -------------------------------------------------------

feature_acf1 <- function(x, freq) {
  acf_value(x, "acf1", freq)
}

feature_acf10 <- function(x, freq) {
  acf_value(x, "acf10", freq)
}

feature_seas_acf1 <- function(x, freq) {
  
  if (freq <= 1) {
    return(NA_real_)
  }
  
  acf_value(x, "seas_acf1", freq)
}

# -------------------------------------------------------
# Box-Cox transformation parameter
# -------------------------------------------------------

feature_lambda_guerrero <- function(x, freq) {
  
  # Guerrero's method requires seasonal subseries.
  if (freq <= 1 || length(x) < 2 * freq) {
    return(NA_real_)
  }
  
  safe_numeric(
    forecast::BoxCox.lambda(
      stats::ts(x, frequency = freq),
      method = "guerrero"
    )
  )
}

# -------------------------------------------------------
# Other time series features
# -------------------------------------------------------

feature_flat_spots <- function(x, freq) {
  
  safe_numeric(
    tsfeatures::flat_spots(
      stats::ts(x, frequency = freq)
    )[["flat_spots"]]
  )
}

feature_stability <- function(x, freq) {
  
  safe_numeric(
    tsfeatures::stability(
      stats::ts(x, frequency = freq)
    )
  )
}

feature_unitroot_pp <- function(x, freq) {
  
  safe_numeric(
    tsfeatures::unitroot_pp(
      stats::ts(x, frequency = freq)
    )
  )
}

feature_unitroot_kpss <- function(x, freq) {
  
  safe_numeric(
    tsfeatures::unitroot_kpss(
      stats::ts(x, frequency = freq)
    )
  )
}

# -------------------------------------------------------
# ETS parameters
# -------------------------------------------------------

feature_ets_parameter <- function(x, parameter, freq) {
  
  series_ts <- stats::ts(x, frequency = freq)
  
  # AAA is seasonal and cannot be fitted at frequency 1.
  # Use ANN for non-seasonal series.
  model_type <- if (freq > 1) "AAA" else "ANN"
  
  fit <- tryCatch(
    forecast::ets(series_ts, model = model_type),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(NA_real_)
  }
  
  if (!parameter %in% names(fit$par)) {
    return(NA_real_)
  }
  
  safe_numeric(fit$par[[parameter]])
}

# -------------------------------------------------------
# Main feature extraction function
# -------------------------------------------------------

# x    : Numeric time series
# h    : Forecasting horizon
# freq : Seasonal frequency (e.g., 1 or 12)
#
# length_horizon = length(x) / h

extract_features <- function(x, h = 12, freq = 12) {
  
  stopifnot(
    is.numeric(x),
    length(h) == 1,
    is.finite(h),
    h > 0,
    length(freq) == 1,
    is.finite(freq),
    freq >= 1,
    freq == as.integer(freq)
  )
  
  x <- scale_ts(as.numeric(x))
  
  if (length(x) < 24) {
    stop(
      "At least 24 non-missing observations are required."
    )
  }
  
  series_ts <- stats::ts(x, frequency = freq)
  
  # Extract the original predictor set
  data.frame(
    
    length_horizon = length(x) / h,
    
    kurtosis = feature_kurtosis(x, freq),
    
    skewness = feature_skewness(x, freq),
    
    non_linearity = feature_nonlinearity(x, freq),
    
    trend_strength = feature_trend_strength(x, freq),
    
    spikiness = feature_spikiness(x, freq),
    
    linearity = feature_linearity(x, freq),
    
    curvature = feature_curvature(x, freq),
    
    residual_acf_1 = feature_residual_acf_1(x, freq),
    
    lambda = feature_lambda_guerrero(x, freq),
    
    seasonal_strength = feature_seasonal_strength(x, freq),
    
    entropy = safe_numeric(
      tsfeatures::entropy(series_ts)
    ),
    
    acf10 = feature_acf10(x, freq),
    
    acf1 = feature_acf1(x, freq),
    
    seas_acf1 = feature_seas_acf1(x, freq),
    
    flat_spots = feature_flat_spots(x, freq),
    
    alpha = feature_ets_parameter(x, "alpha", freq),
    
    hurst_exp = feature_hurst_empirical(x),
    
    beta = feature_ets_parameter(x, "beta", freq),
    
    gamma = feature_ets_parameter(x, "gamma", freq),
    
    crossing_points = safe_numeric(
      tsfeatures::crossing_points(series_ts)
    ),
    
    outlier_prop = safe_numeric(
      get(
        "outlierinclude_mdrmd",
        envir = asNamespace("tsfeatures")
      )(series_ts)
    ),
    
    walker_prop = safe_numeric(
      get(
        "walker_propcross",
        envir = asNamespace("tsfeatures")
      )(series_ts)
    ),
    
    taures = safe_numeric(
      get(
        "localsimple_taures",
        envir = asNamespace("tsfeatures")
      )(series_ts)
    ),
    
    stability = feature_stability(x, freq),
    
    unitroot_pp = feature_unitroot_pp(x, freq),
    
    unitroot_kpss = feature_unitroot_kpss(x, freq),
    
    check.names = FALSE
  )
}

