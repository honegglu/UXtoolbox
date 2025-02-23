#' Bayesian Survival Analysis for UX Retention Studies
#'
#' Performs Bayesian survival analysis using a joint model to estimate retention rates over time.
#'
#' @param data A data frame containing UX retention variables.
#' @param formula A formula specifying survival time and predictors (e.g., `Surv(time, event) ~ age + experience`).
#' @param n_samples Number of posterior samples (default is 4000).
#'
#' @return A Bayesian survival model summary.
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
bayesian_survival_analysis <- function(data, formula, n_samples = 4000) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) install.packages("rstanarm")
  if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")

  library(rstanarm)
  library(survival)

  # Fit a Bayesian survival model using stan_jm()
  model <- stan_jm(
    formulaLong = as.formula(formula),
    dataLong = data,
    time_var = "time",
    family = gaussian(),
    chains = 4,
    iter = n_samples,
    seed = 1234
  )

  return(summary(model))
}
