#' Structural Equation Modeling (SEM) for UX Data
#'
#' Performs Structural Equation Modeling (SEM) to examine relationships between UX variables.
#'
#' @param data A data frame containing UX variables.
#' @param model_string A character string specifying the SEM model in lavaan syntax.
#' @param estimator The estimation method (default is "MLR" for robust maximum likelihood).
#'
#' @return A summary of the fitted SEM model.
#' @export
#'
#' @examples
#' library(lavaan)
#' test_data <- data.frame(
#'   usability = c(3.5, 4.0, 4.2, 3.8, 4.5, 3.9, 4.1, 4.3, 4.0, 4.2),
#'   engagement = c(2.9, 3.1, 3.5, 3.0, 3.8, 3.2, 3.4, 3.6, 3.3, 3.7),
#'   satisfaction = c(4.0, 4.2, 4.5, 4.1, 4.7, 4.3, 4.4, 4.6, 4.2, 4.5)
#' )
#'
#' model <- "
#'   engagement ~ usability
#'   satisfaction ~ usability + engagement
#' "
#' sem_analysis(test_data, model)
sem_analysis <- function(data, model_string, estimator = "MLR") {
  if (!requireNamespace("lavaan", quietly = TRUE)) install.packages("lavaan")

  library(lavaan)

  model_fit <- sem(model_string, data = data, estimator = estimator)
  return(summary(model_fit, fit.measures = TRUE, standardized = TRUE))
}
