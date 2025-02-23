#' Bayesian Linear Regression for UX Predictions
#'
#' Performs Bayesian linear regression to model UX outcomes with credible intervals and effect size estimation.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the regression model (e.g., `completion_time ~ experience + age`).
#' @param n_samples Number of posterior samples (default is 10000).
#' @param priors A list of priors for coefficients (default is weakly informative).
#'
#' @return A list containing the Bayesian regression model summary, credible intervals, and posterior distributions.
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

bayesian_linear_regression <- function(data, formula, n_samples = 10000, priors = normal(0, 1)) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) install.packages("rstanarm")
  if (!requireNamespace("bayestestR", quietly = TRUE)) install.packages("bayestestR")

  library(rstanarm)
  library(bayestestR)

  # Fit Bayesian linear regression with weakly informative priors
  model <- stan_glm(formula, data = data, family = gaussian(),
                    iter = n_samples, prior = priors, refresh = 0)

  # Extract posterior samples
  posterior_samples <- as.data.frame(model)

  # Compute credible intervals for coefficients
  ci_bounds <- bayestestR::ci(posterior_samples, ci = 0.95)

  return(list(
    Model_Summary = summary(model),
    Posterior_Samples = posterior_samples,
    Credible_Intervals = ci_bounds
  ))
}
