# Configure Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Type conversion helper functions
char <- function(x) as.character(x)
num <- function(x) as.numeric(x)

#' Summarize MCMC results
#' 
#' Calculate summary statistics from JAGS MCMC matrix including means,
#' standard deviations, and various credible intervals
#' @param x MCMC matrix with parameters in columns
#' @return Data frame with summary statistics for each parameter
jags_summary <- function(x) {
  data.frame(
    var = colnames(x),
    mean = apply(x, 2, mean),
    sd = apply(x, 2, sd),
    q95lo = apply(x, 2, quantile, probs = 0.025),
    q95hi = apply(x, 2, quantile, probs = 0.975),
    q90lo = apply(x, 2, quantile, probs = 0.05),
    q90hi = apply(x, 2, quantile, probs = 0.95),
    q80lo = apply(x, 2, quantile, probs = 0.10),
    q80hi = apply(x, 2, quantile, probs = 0.90),
    stringsAsFactors = FALSE
  )
}

#' Check Coalition Majority
#' 
#' Determine if a coalition has a majority of votes among parties above threshold
#' @param share Vote shares for coalition parties
#' @param share_above_hurdle Total vote share of parties above threshold
#' @return Logical indicating if coalition has majority
coal_majo <- function(share, share_above_hurdle) {
  if(any(share < 0.05)) {
    return(FALSE)
  } else {
    return(sum(share)/share_above_hurdle > 0.5)
  }
}

#' Calculate Root Mean Square Error
#' @param pred Vector of predictions
#' @param obs Vector of observed values
#' @return RMSE value
rmse <- function(pred, obs) {
  sqrt(mean((pred - obs) ^ 2))
}

#' Calculate Log Ratio Transformation
#' 
#' Transform percentage values to log ratios, handling zero values
#' @param x Numeric vector of percentages
#' @return Vector of log ratios
log_ratio <- function(x) {
  x <- x / 100                           # Scale to proportions
  x <- ifelse(x == 0, x + 0.01, x)      # Handle zeros
  log(x/(1-x))                          # Calculate log ratio
}