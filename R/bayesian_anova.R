#' Bayesian ANOVA for UX Group Comparisons
#'
#' Performs Bayesian ANOVA to compare three or more groups on a UX metric.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the model (e.g., `completion_time ~ UI_version`).
#' @param n_samples Number of posterior samples (default is 4000).
#'
#' @return A Bayesian ANOVA summary.
#' @export
#'
#' @examples
#' library(BayesFactor)
#' test_data <- data.frame(
#'   completion_time = c(5.1, 6.3, 5.5, 4.8, 7.2, 6.0, 5.4, 4.9, 7.0, 6.5),
#'   UI_version = factor(rep(c("A", "B", "C"), length.out = 10))
#' )
#' bayesian_anova(test_data, completion_time ~ UI_version)
bayesian_anova <- function(data, formula, n_samples = 4000) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) install.packages("BayesFactor")

  library(BayesFactor)

  bf_model <- anovaBF(formula, data = data, iterations = n_samples)
  return(bf_model)
}
