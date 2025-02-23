#' Bootstrap Confidence Interval for the Mean in UX Studies
#'
#' Uses bootstrapping to compute a confidence interval for the mean of a numeric UX metric.
#' Provides percentile, basic, and bias-corrected (BCa) confidence intervals.
#'
#' @param data A numeric vector representing the UX metric.
#' @param n_boot The number of bootstrap samples (default is 2000).
#' @param conf_level The confidence level for the interval (default is 0.95).
#'
#' @return A list containing the bootstrap confidence intervals (percentile, basic, and BCa).
#' @export
#'
#' @examples
#' test_data <- rnorm(100, mean = 50, sd = 10)
#' bootstrap_mean_ci(test_data, n_boot = 2000, conf_level = 0.95)

bootstrap_mean_ci <- function(data, n_boot = 2000, conf_level = 0.95) {
  if (!requireNamespace("boot", quietly = TRUE)) install.packages("boot")

  library(boot)

  boot_mean <- function(data, i) mean(data[i])
  boot_out <- boot(data, statistic = boot_mean, R = n_boot)

  # Compute multiple types of confidence intervals
  ci_perc <- boot.ci(boot_out, type = "perc", conf = conf_level)
  ci_basic <- boot.ci(boot_out, type = "basic", conf = conf_level)
  ci_bca <- boot.ci(boot_out, type = "bca", conf = conf_level)

  return(list(
    Percentile_CI = ci_perc,
    Basic_CI = ci_basic,
    BCa_CI = ci_bca
  ))
}
