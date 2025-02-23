#' Bayesian Logistic Regression for UX Predictions
#'
#' Performs Bayesian logistic regression to estimate the probability of a binary UX outcome.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the logistic regression model (e.g., `task_success ~ experience + age`).
#' @param n_samples The number of samples to draw from the posterior distribution (default is 10000).
#'
#' @return A posterior distribution of coefficients and predicted probabilities.
#' @export
#'
#' @examples
#' library(rstanarm)
#' test_data <- data.frame(task_success = c(1, 0, 1, 1, 0),
#'                         experience = c(1, 2, 3, 4, 5),
#'                         age = c(22, 30, 25, 40, 35))
#' bayesian_logistic_regression(test_data, task_success ~ experience + age)
bayesian_logistic_regression <- function(data, formula, n_samples = 10000) {
  model <- rstanarm::stan_glm(formula, data = data, family = binomial, iter = n_samples, refresh = 0)
  return(summary(model))
}
