#' Bayesian t-Test for UX Comparisons
#'
#' Performs a Bayesian t-test to estimate the probability that two groups have different means.
#'
#' @param group1 A numeric vector representing values from the first group.
#' @param group2 A numeric vector representing values from the second group.
#' @param n_samples The number of samples to draw from the posterior distribution (default is 10000).
#'
#' @return The probability that the mean of group1 is greater than group2.
#' @export
#'
#' @examples
#' set.seed(42)
#' group1 <- rnorm(30, mean = 50, sd = 10)
#' group2 <- rnorm(30, mean = 45, sd = 10)
#' bayesian_ttest(group1, group2)
bayesian_ttest <- function(group1, group2, n_samples = 10000) {
  mu1 <- mean(group1)
  mu2 <- mean(group2)
  sd1 <- sd(group1)
  sd2 <- sd(group2)
  n1 <- length(group1)
  n2 <- length(group2)

  post_mu1 <- rnorm(n_samples, mean = mu1, sd = sd1 / sqrt(n1))
  post_mu2 <- rnorm(n_samples, mean = mu2, sd = sd2 / sqrt(n2))

  prob_mu1_greater <- mean(post_mu1 > post_mu2)
  return(prob_mu1_greater)
}
