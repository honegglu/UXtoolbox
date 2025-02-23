#' Bayesian t-Test for UX Comparisons
#'
#' Performs a Bayesian t-test to estimate the probability that two UX conditions have different means.
#' Provides posterior distributions, credible intervals, and effect size estimation.
#'
#' @param group1 A numeric vector representing values from the first UX condition.
#' @param group2 A numeric vector representing values from the second UX condition.
#' @param n_samples The number of samples to draw from the posterior distribution (default is 10000).
#' @param ci The credible interval width (default is 0.95).
#'
#' @return A list containing the probability that group1 has a greater mean, posterior distributions,
#'         credible intervals, and Bayesian effect size estimation.
#' @export
#'
#' @examples
#' set.seed(42)
#' group1 <- rnorm(30, mean = 50, sd = 10)
#' group2 <- rnorm(30, mean = 45, sd = 10)
#' bayesian_ttest(group1, group2)

bayesian_ttest <- function(group1, group2, n_samples = 10000, ci = 0.95) {
  if (!requireNamespace("bayestestR", quietly = TRUE)) install.packages("bayestestR")

  library(bayestestR)

  mu1 <- mean(group1)
  mu2 <- mean(group2)
  sd1 <- sd(group1)
  sd2 <- sd(group2)
  n1 <- length(group1)
  n2 <- length(group2)

  # Generate posterior distributions
  post_mu1 <- rnorm(n_samples, mean = mu1, sd = sd1 / sqrt(n1))
  post_mu2 <- rnorm(n_samples, mean = mu2, sd = sd2 / sqrt(n2))

  # Compute probability that group1 mean is greater than group2
  prob_mu1_greater <- mean(post_mu1 > post_mu2)

  # Compute posterior difference
  diff_posterior <- post_mu1 - post_mu2

  # Compute credible interval
  ci_bounds <- bayestestR::ci(diff_posterior, ci = ci)

  # Compute effect size (Cohen’s d)
  pooled_sd <- sqrt(((sd1^2) / n1) + ((sd2^2) / n2))
  effect_size <- (mu1 - mu2) / pooled_sd
  bayesian_d <- bayestestR::ci(effect_size, ci = ci)

  return(list(
    Probability_Group1_Greater = prob_mu1_greater,
    Posterior_Distribution = diff_posterior,
    Credible_Interval = ci_bounds,
    Bayesian_Effect_Size = bayesian_d
  ))
}
