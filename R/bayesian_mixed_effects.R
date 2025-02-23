#' Bayesian Mixed-Effects Model for UX Data
#'
#' Performs Bayesian mixed-effects modeling using a hierarchical structure to analyze repeated measures or nested data.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the mixed model (e.g., `completion_time ~ condition + (1 | user_id)`).
#' @param n_samples Number of posterior samples (default is 4000).
#'
#' @return A Bayesian mixed-effects model summary.
#' @export
#'
#' @examples
#' library(rstanarm)
#' test_data <- data.frame(
#'   completion_time = c(5.1, 6.3, 5.5, 4.8, 7.2, 6.0, 5.4, 4.9, 7.0, 6.5),
#'   condition = factor(rep(c("A", "B"), length.out = 10)),
#'   user_id = factor(rep(1:5, each = 2))
#' )
#' bayesian_mixed_effects(test_data, completion_time ~ condition + (1 | user_id))
bayesian_mixed_effects <- function(data, formula, n_samples = 4000) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) install.packages("rstanarm")
  if (!requireNamespace("lme4", quietly = TRUE)) install.packages("lme4")

  library(rstanarm)
  library(lme4)

  model <- stan_glmer(formula, data = data, family = gaussian(), iter = n_samples, refresh = 0)
  return(summary(model))
}
