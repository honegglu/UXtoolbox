#' Bootstrap Confidence Interval for the Median
#'
#' Uses bootstrapping to compute a confidence interval for the median of a numeric variable.
#'
#' @param data A numeric vector representing the data.
#' @param n_boot The number of bootstrap samples (default is 1000).
#' @param conf_level The confidence level for the interval (default is 0.95).
#'
#' @return A list containing the bootstrap confidence interval.
#' @export
#'
#' @examples
#' test_data <- rnorm(100, mean = 50, sd = 10)
#' bootstrap_median_ci(test_data, n_boot = 2000, conf_level = 0.95)
bootstrap_median_ci <- function(data, n_boot = 1000, conf_level = 0.95) {
  boot_median <- function(data, i) median(data[i])
  boot_out <- boot::boot(data, statistic = boot_median, R = n_boot)
  boot::boot.ci(boot_out, type = "perc", conf = conf_level)
}
