#' Bayesian Survival Analysis for UX Retention Studies
#'
#' Performs Bayesian survival analysis to estimate retention rates and time-to-event probabilities in UX studies.
#' Provides posterior distributions, hazard ratios, and credible intervals.
#'
#' @param data A data frame containing UX retention variables.
#' @param formula A formula specifying survival time and predictors (e.g., `Surv(time, event) ~ age + experience`).
#' @param n_samples Number of posterior samples (default is 10000).
#' @param priors A list of priors for coefficients (default is weakly informative).
#'
#' @return A list containing model summary, posterior distributions, hazard ratios, and credible intervals.
#' @export
#'
#' @examples
#' library(rstanarm)
#' library(survival)
#' test_data <- data.frame(
#'   time = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
#'   event = c(1, 1, 0, 1, 0, 1, 1, 0, 1, 0),
#'   age = c(25, 30, 28, 35, 40, 26, 32, 29, 38, 33),
#'   experience = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' )
#' bayesian_survival_analysis(test_data, Surv(time, event) ~ age + experience)

bayesian_survival_analysis <- function(data, formula, n_samples = 10000, priors = normal(0, 1)) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) install.packages("rstanarm")
  if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
  if (!requireNamespace("bayestestR", quietly = TRUE)) install.packages("bayestestR")

  library(rstanarm)
  library(survival)
  library(bayestestR)

  # Fit a Bayesian survival model using stan_surv
  model <- stan_surv(
    formula = formula,
    data = data,
    basehaz = "exp",  # Exponential hazard function
    iter = n_samples,
    prior = priors,
    refresh = 0
  )

  # Extract posterior samples
  posterior_samples <- as.data.frame(model)

  # Compute hazard ratios
  hazard_ratios <- exp(posterior_samples)

  # Compute credible intervals
  ci_bounds <- bayestestR::ci(posterior_samples, ci = 0.95)

  return(list(
    Model_Summary = summary(model),
    Posterior_Samples = posterior_samples,
    Hazard_Ratios = hazard_ratios,
    Credible_Intervals = ci_bounds
  ))
}
