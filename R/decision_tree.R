#' Decision Tree Model for UX Predictions
#'
#' Fits a decision tree model to predict UX outcomes.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the decision tree model (e.g., `task_success ~ experience + age`).
#'
#' @return A trained decision tree model.
#' @export
#'
#' @examples
#' library(rpart)
#' test_data <- data.frame(task_success = c(1, 0, 1, 1, 0),
#'                         experience = c(1, 2, 3, 4, 5),
#'                         age = c(22, 30, 25, 40, 35))
#' decision_tree(test_data, task_success ~ experience + age)
decision_tree <- function(data, formula) {
  model <- rpart::rpart(formula, data = data, method = "class")
  return(model)
}
