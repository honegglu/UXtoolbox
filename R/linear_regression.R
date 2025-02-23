#' Linear Regression Model for UX Metrics
#'
#' Fits a linear regression model to predict UX outcomes.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the regression model (e.g., `task_time ~ experience`).
#'
#' @return A summary of the regression model.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         experience = c(1, 2, 3, 4, 5))
#' linear_regression(test_data, task_time ~ experience)
linear_regression <- function(data, formula) {
  model <- lm(formula, data = data)
  summary(model)
}
