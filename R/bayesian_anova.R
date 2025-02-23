#' Bayesian ANOVA for UX Group Comparisons
#'
#' Performs Bayesian ANOVA to compare three or more groups on a UX metric.
#' Provides Bayes Factors, credible intervals, and posterior distributions.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the model (e.g., `completion_time ~ UI_version`).
#' @param n_samples Number of posterior samples (default is 10000).
#' @param prior_scale The scale of the Cauchy prior (default is 0.5).
#'
#' @return A list containing the Bayesian ANOVA summary, posterior distributions, and credible intervals.
#' @export
#'
#' @examples
#' library(BayesFactor)
#' test_data <- data.frame(
#'   completion_time = c(5.1, 6.3, 5.5, 4.8, 7.2, 6.0, 5.4, 4.9, 7.0, 6.5),
#'   UI_version = factor(rep(c("A", "B", "C"), length.out = 10))
#' )
#' bayesian_anova(test_data, completion_time ~ UI_version)

bayesian_anova <- function(data, formula, n_samples = 10000, prior_scale = 0.5) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) install.packages("BayesFactor")
  if (!requireNamespace("bayestestR", quietly = TRUE)) install.packages("bayestestR")

  library(BayesFactor)
  library(bayestestR)

  # Run Bayesian ANOVA
  bf_model <- anovaBF(formula, data = data, iterations = n_samples, rscaleFixedEffects = prior_scale)

  # Extract Bayes Factor and posterior distributions
  bf_summary <- summary(bf_model)
  posterior_samples <- posterior(bf_model, iterations = n_samples)

  # Compute credible intervals
  ci_bounds <- bayestestR::ci(posterior_samples, ci = 0.95)

  return(list(
    Bayes_Factor = bf_summary,
    Posterior_Samples = posterior_samples,
    Credible_Intervals = ci_bounds
  ))
}
