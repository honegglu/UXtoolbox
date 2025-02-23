#' Bootstrap Confidence Interval for the Mean
#'
#' Uses bootstrapping to compute a confidence interval for the mean of a numeric variable.
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
#' bootstrap_mean_ci(test_data, n_boot = 2000, conf_level = 0.95)
bootstrap_mean_ci <- function(data, n_boot = 1000, conf_level = 0.95) {
  boot_mean <- function(data, i) mean(data[i])
  boot_out <- boot::boot(data, statistic = boot_mean, R = n_boot)
  boot::boot.ci(boot_out, type = "perc", conf = conf_level)
}
