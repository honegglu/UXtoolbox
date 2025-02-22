#' Get a confidence interval for the mean using bootstrapping
#' @param data A numeric vector
#' @param n_boot Number of bootstrap samples
#' @return Bootstrap confidence interval
#' @export
bootstrap_mean_ci <- function(data, n_boot = 1000) {
  boot_mean <- function(data, i) mean(data[i])
  boot_out <- boot(data, statistic = boot_mean, R = n_boot)
  boot.ci(boot_out, type = "perc")
}
