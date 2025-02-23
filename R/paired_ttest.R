#' Paired Samples t-Test for UX Comparisons
#'
#' Performs a paired t-test to compare two related samples (e.g., before and after a UX change).
#' Provides a visualization of the paired differences.
#'
#' @param data A data frame containing UX metrics.
#' @param pre_col The column name for pre-test values.
#' @param post_col The column name for post-test values.
#'
#' @return A list containing the paired t-test results, effect size, and a visualization.
#' @export
#'
#' @examples
#' test_data <- data.frame(Before = c(55, 60, 65, 70, 75),
#'                         After = c(60, 63, 68, 72, 78))
#' paired_ttest(test_data, "Before", "After")

paired_ttest <- function(data, pre_col, post_col) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("effsize", quietly = TRUE)) install.packages("effsize")

  library(ggplot2)
  library(effsize)

  # Perform Paired t-Test
  test_result <- t.test(data[[pre_col]], data[[post_col]], paired = TRUE)

  # Compute Effect Size (Cohen's d for paired samples)
  effect_size <- cohen.d(data[[pre_col]], data[[post_col]], paired = TRUE)

  # Create visualization of before vs. after values
  plot_data <- data.frame(
    ID = 1:nrow(data),
    Before = data[[pre_col]],
    After = data[[post_col]]
  )

  paired_plot <- ggplot(plot_data, aes(x = ID)) +
    geom_line(aes(y = Before, group = ID), color = "blue", alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = After, group = ID), color = "red", alpha = 0.6) +
    geom_point(aes(y = Before), color = "blue", size = 3) +
    geom_point(aes(y = After), color = "red", size = 3) +
    labs(title = "Paired t-Test: Before vs. After",
         x = "Participant ID",
         y = "Values") +
    theme_minimal()

  return(list(
    Test_Result = test_result,
    Effect_Size = effect_size,
    Paired_Comparison_Plot = paired_plot
  ))
}
