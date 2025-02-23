#' Independent Samples t-Test for UX Comparisons
#'
#' Performs an independent t-test to compare two groups and provides a visualization of the distributions.
#'
#' @param data A data frame containing UX metrics.
#' @param group_col The column name for the grouping variable (factor with 2 levels).
#' @param value_col The column name for the numeric values to compare.
#'
#' @return A list containing the t-test results and a density plot visualization.
#' @export
#'
#' @examples
#' test_data <- data.frame(Group = rep(c("A", "B"), each = 5),
#'                         Score = c(10, 12, 14, 16, 18, 9, 11, 13, 15, 17))
#' compare_groups_ttest(test_data, "Group", "Score")

compare_groups_ttest <- function(data, group_col, value_col) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  
  library(ggplot2)
  
  # Perform t-test
  test_result <- t.test(data[[value_col]] ~ data[[group_col]], data = data)
  
  # Create density plot visualization
  density_plot <- ggplot(data, aes(x = data[[value_col]], fill = as.factor(data[[group_col]]))) +
    geom_density(alpha = 0.5) +
    labs(title = "Independent t-Test: Group Distributions",
         x = "Values",
         fill = "Group") +
    theme_minimal()
  
  return(list(
    Test_Result = test_result,
    Density_Plot = density_plot
  ))
}
