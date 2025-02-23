#' Random Forest Model for UX Predictions
#'
#' Fits a random forest model to predict UX outcomes.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the random forest model (e.g., `task_success ~ experience + age`).
#' @param n_trees Number of trees in the random forest (default is 100).
#'
#' @return A trained random forest model.
#' @export
#'
#' @examples
#' library(randomForest)
#' test_data <- data.frame(task_success = factor(c(1, 0, 1, 1, 0)),
#'                         experience = c(1, 2, 3, 4, 5),
#'                         age = c(22, 30, 25, 40, 35))
#' random_forest(test_data, task_success ~ experience + age, n_trees = 200)
random_forest <- function(data, formula, n_trees = 100) {
  model <- randomForest::randomForest(formula, data = data, ntree = n_trees)
  return(model)
}

