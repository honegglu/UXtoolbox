#' Logistic Regression Model for UX Predictions
#'
#' Fits a logistic regression model to predict binary UX outcomes.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the logistic regression model (e.g., `task_success ~ experience`).
#'
#' @return A summary of the logistic regression model.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_success = c(1, 0, 1, 1, 0),
#'                         experience = c(1, 2, 3, 4, 5))
#' logistic_regression(test_data, task_success ~ experience)
logistic_regression <- function(data, formula) {
  model <- glm(formula, data = data, family = binomial)
  summary(model)
}
