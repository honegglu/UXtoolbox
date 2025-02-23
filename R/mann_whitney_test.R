#' Mann-Whitney U Test (Wilcoxon Rank-Sum Test) for UX Comparisons
#'
#' Performs a Mann-Whitney U test to compare two independent groups when data is not normally distributed.
#' Provides a boxplot visualization of group differences.
#'
#' @param data A data frame containing UX metrics.
#' @param group_col The column name for the grouping variable (factor with 2 levels).
#' @param value_col The column name for the numeric values to compare.
#'
#' @return A list containing the Mann-Whitney U test results, effect size, and a boxplot visualization.
#' @export
#'
#' @examples
#' test_data <- data.frame(Group = rep(c("A", "B"), each = 5),
#'                         Score = c(10, 20, 30, 40, 50, 12, 22, 32, 42, 52))
#' mann_whitney_test(test_data, "Group", "Score")

mann_whitney_test <- function(data, group_col, value_col) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("effsize", quietly = TRUE)) install.packages("effsize")

  library(ggplot2)
  library(effsize)

  # Ensure input columns are properly formatted
  data[[group_col]] <- as.factor(data[[group_col]])

  # Perform Mann-Whitney U Test
  test_result <- wilcox.test(data[[value_col]] ~ data[[group_col]], data = data)

  # Compute effect size (Cliff’s Delta)
  effect_size <- cliff.delta(data[[value_col]][data[[group_col]] == levels(data[[group_col]])[1]],
                             data[[value_col]][data[[group_col]] == levels(data[[group_col]])[2]])

  # Create boxplot visualization
  boxplot_plot <- ggplot(data, aes(x = data[[group_col]], y = data[[value_col]], fill = data[[group_col]])) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(shape = 16, position = position_jitter(0.2), alpha = 0.5) +
    labs(title = "Mann-Whitney U Test: Group Differences",
         x = "Group",
         y = "Values") +
    theme_minimal() +
    theme(legend.position = "none")

  return(list(
    Test_Result = test_result,
    Effect_Size = effect_size,
    Boxplot = boxplot_plot
  ))
}
