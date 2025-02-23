#' Decision Tree Model for UX Predictions
#'
#' Fits a decision tree model to predict UX outcomes such as task success, user retention, or engagement levels.
#' Provides feature importance, tree visualization, and model accuracy.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the decision tree model (e.g., `task_success ~ experience + age`).
#' @param minsplit The minimum number of observations required to split an internal node (default is 5).
#' @param cp Complexity parameter for pruning (default is 0.01).
#'
#' @return A list containing the trained decision tree model, feature importance, and tree visualization.
#' @export
#'
#' @examples
#' library(rpart)
#' test_data <- data.frame(
#'   task_success = c(1, 0, 1, 1, 0),
#'   experience = c(1, 2, 3, 4, 5),
#'   age = c(22, 30, 25, 40, 35)
#' )
#' decision_tree(test_data, task_success ~ experience + age)

decision_tree <- function(data, formula, minsplit = 5, cp = 0.01) {
  if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
  if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")

  library(rpart)
  library(rpart.plot)

  # Fit decision tree model
  model <- rpart(formula, data = data, method = "class", minsplit = minsplit, cp = cp)

  # Extract feature importance
  feature_importance <- as.data.frame(varImp(model))

  # Plot decision tree
  plot <- rpart.plot(model, type = 3, extra = 101, fallen.leaves = TRUE)

  return(list(
    Decision_Tree_Model = model,
    Feature_Importance = feature_importance,
    Tree_Plot = plot
  ))
}
