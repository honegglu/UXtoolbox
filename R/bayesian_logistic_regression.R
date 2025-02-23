#' Bayesian Logistic Regression for UX Predictions
#'
#' Performs Bayesian logistic regression to estimate the probability of a binary UX outcome.
#' Provides posterior distributions, credible intervals, and predicted probabilities.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the logistic regression model (e.g., `task_success ~ experience + age`).
#' @param n_samples The number of samples to draw from the posterior distribution (default is 10000).
#' @param priors A list of priors for coefficients (default is weakly informative).
#'
#' @return A list containing model summary, posterior distributions, credible intervals, and predicted probabilities.
#' @export
#'
#' @examples
#' library(rstanarm)
#' test_data <- data.frame(
#'   task_success = c(1, 0, 1, 1, 0),
#'   experience = c(1, 2, 3, 4, 5),
#'   age = c(22, 30, 25, 40, 35)
#' )
#' bayesian_logistic_regression(test_data, task_success ~ experience + age)

bayesian_logistic_regression <- function(data, formula, n_samples = 10000, priors = normal(0, 1)) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) install.packages("rstanarm")
  if (!requireNamespace("bayestestR", quietly = TRUE)) install.packages("bayestestR")

  library(rstanarm)
  library(bayestestR)

  # Fit Bayesian logistic regression with weakly informative priors
  model <- stan_glm(formula, data = data, family = binomial(),
                    iter = n_samples, prior = priors, refresh = 0)

  # Extract posterior samples
  posterior_samples <- as.data.frame(model)

  # Compute credible intervals for coefficients
  ci_bounds <- bayestestR::ci(posterior_samples, ci = 0.95)

  # Compute predicted probabilities
  predicted_probs <- posterior_epred(model)

  return(list(
    Model_Summary = summary(model),
    Posterior_Samples = posterior_samples,
    Credible_Intervals = ci_bounds,
    Predicted_Probabilities = predicted_probs
  ))
}
