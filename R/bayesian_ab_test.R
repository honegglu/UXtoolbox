#' Bayesian A/B Test for UX Comparisons
#'
#' Performs a Bayesian A/B test to estimate the probability that one condition (A) is better than another (B).
#'
#' @param successes A vector with the number of successes in each group (A, B).
#' @param trials A vector with the total trials in each group (A, B).
#' @param n_samples The number of samples to draw from the posterior distribution (default is 10000).
#'
#' @return The probability that condition A is better than condition B.
#' @export
#'
#' @examples
#' bayesian_ab_test(successes = c(30, 20), trials = c(100, 100))
bayesian_ab_test <- function(successes, trials, n_samples = 10000) {
  a_alpha <- successes[1] + 1
  a_beta <- trials[1] - successes[1] + 1
  b_alpha <- successes[2] + 1
  b_beta <- trials[2] - successes[2] + 1

  p_a <- rbeta(n_samples, a_alpha, a_beta)
  p_b <- rbeta(n_samples, b_alpha, b_beta)

  prob_a_better <- mean(p_a > p_b)
  return(prob_a_better)
}
