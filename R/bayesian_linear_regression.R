#' Bayesian Linear Regression for UX Predictions
#'
#' Performs Bayesian linear regression to model UX outcomes.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the regression model (e.g., `completion_time ~ experience + age`).
#' @param n_samples Number of posterior samples (default is 4000).
#'
#' @return A Bayesian linear regression model summary.
#' @export
#'
#' @examples
#' library(rstanarm)
#' test_data <- data.frame(
#'   completion_time = c(5.1, 6.3, 5.5, 4.8, 7.2, 6.0, 5.4, 4.9, 7.0, 6.5),
#'   experience = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'   age = c(25, 30, 28, 35, 40, 26, 32, 29, 38, 33)
#' )
#' bayesian_linear_regression(test_data, completion_time ~ experience + age)
bayesian_linear_regression <- function(data, formula, n_samples = 4000) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) install.packages("rstanarm")

  library(rstanarm)

  model <- stan_glm(formula, data = data, family = gaussian(), iter = n_samples, refresh = 0)
  return(summary(model))
}
