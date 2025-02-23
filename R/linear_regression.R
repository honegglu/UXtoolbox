#' Linear Regression Model for UX Metrics
#'
#' Fits a linear regression model to predict UX outcomes and provides diagnostic plots.
#'
#' @param data A data frame containing UX variables.
#' @param formula A formula specifying the regression model (e.g., `task_time ~ experience`).
#'
#' @return A list containing the regression summary and diagnostic plots.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         experience = c(1, 2, 3, 4, 5))
#' linear_regression(test_data, task_time ~ experience)

linear_regression <- function(data, formula) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

  library(ggplot2)

  # Fit linear model
  model <- lm(formula, data = data)
  model_summary <- summary(model)

  # Create diagnostic plots
  residual_plot <- ggplot(data, aes(x = fitted(model), y = resid(model))) +
    geom_point(color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Residuals vs. Fitted", x = "Fitted Values", y = "Residuals") +
    theme_minimal()

  qq_plot <- ggplot(data, aes(sample = resid(model))) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Q-Q Plot for Residuals") +
    theme_minimal()

  return(list(
    Model_Summary = model_summary,
    Residuals_vs_Fitted_Plot = residual_plot,
    QQ_Plot = qq_plot
  ))
}
