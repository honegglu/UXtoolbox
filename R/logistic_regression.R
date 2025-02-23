#' Logistic Regression Model for UX Predictions
#'
#' Fits a logistic regression model to predict binary UX outcomes and provides diagnostic plots.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the logistic regression model (e.g., `task_success ~ experience`).
#'
#' @return A list containing the logistic regression summary and diagnostic plots.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_success = c(1, 0, 1, 1, 0),
#'                         experience = c(1, 2, 3, 4, 5))
#' logistic_regression(test_data, task_success ~ experience)

logistic_regression <- function(data, formula) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

  library(ggplot2)

  # Fit logistic model
  model <- glm(formula, data = data, family = binomial)
  model_summary <- summary(model)

  # Get predicted probabilities
  data$predicted_prob <- predict(model, type = "response")

  # Plot predicted probabilities
  prob_plot <- ggplot(data, aes(x = predicted_prob, fill = as.factor(data[[all.vars(formula)[1]]]))) +
    geom_histogram(position = "identity", alpha = 0.7, bins = 10) +
    labs(title = "Predicted Probabilities Distribution",
         x = "Predicted Probability",
         fill = "Outcome") +
    theme_minimal()

  return(list(
    Model_Summary = model_summary,
    Predicted_Probabilities_Plot = prob_plot
  ))
}
