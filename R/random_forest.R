#' Random Forest Model for UX Predictions
#'
#' Fits a random forest model to predict UX outcomes such as task success, engagement, or user behavior.
#' Provides feature importance, model accuracy, and a visualization of variable importance.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the random forest model (e.g., `task_success ~ experience + age`).
#' @param n_trees Number of trees in the random forest (default is 100).
#'
#' @return A list containing the trained random forest model, feature importance, and an importance plot.
#' @export
#'
#' @examples
#' library(randomForest)
#' test_data <- data.frame(
#'   task_success = factor(c(1, 0, 1, 1, 0)),
#'   experience = c(1, 2, 3, 4, 5),
#'   age = c(22, 30, 25, 40, 35)
#' )
#' random_forest(test_data, task_success ~ experience + age, n_trees = 200)

random_forest <- function(data, formula, n_trees = 100) {
  if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

  library(randomForest)
  library(ggplot2)

  # Fit random forest model
  model <- randomForest(formula, data = data, ntree = n_trees, importance = TRUE)

  # Compute feature importance
  importance_scores <- importance(model)
  importance_df <- data.frame(Feature = rownames(importance_scores), Importance = importance_scores[, 1])

  # Create feature importance plot
  importance_plot <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_col(fill = "blue", alpha = 0.7) +
    coord_flip() +
    labs(title = "Feature Importance", x = "Features", y = "Importance Score") +
    theme_minimal()

  return(list(
    Random_Forest_Model = model,
    Feature_Importance = importance_scores,
    Importance_Plot = importance_plot
  ))
}
