#' Permutation Test for Group Differences in UX Metrics
#'
#' Uses a permutation test to compare two independent groups without assuming normality.
#' Provides a histogram of the permutation distribution.
#'
#' @param data A data frame containing UX metrics.
#' @param group_col The column name for the grouping variable (factor with 2 levels).
#' @param value_col The column name for the numeric values to compare.
#' @param n_perms The number of permutations to perform (default is 1000).
#'
#' @return A list containing the observed difference, p-value, and a permutation distribution plot.
#' @export
#'
#' @examples
#' test_data <- data.frame(Group = rep(c("A", "B"), each = 10),
#'                         Score = c(10, 12, 15, 18, 20, 25, 30, 35, 40, 45,
#'                                   12, 14, 17, 19, 22, 28, 32, 36, 41, 44))
#' permutation_test(test_data, "Group", "Score", n_perms = 2000)

permutation_test <- function(data, group_col, value_col, n_perms = 1000) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

  library(ggplot2)

  # Compute observed difference
  group_levels <- unique(data[[group_col]])
  obs_diff <- abs(mean(data[[value_col]][data[[group_col]] == group_levels[1]]) -
                    mean(data[[value_col]][data[[group_col]] == group_levels[2]]))

  # Generate permutation distribution
  perm_diffs <- replicate(n_perms, {
    shuffled <- sample(data[[value_col]])
    abs(mean(shuffled[1:(nrow(data)/2)]) - mean(shuffled[(nrow(data)/2 + 1):nrow(data)]))
  })

  # Compute p-value
  p_value <- mean(perm_diffs >= obs_diff)

  # Plot permutation distribution
  perm_plot <- ggplot(data.frame(perm_diffs), aes(x = perm_diffs)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7, color = "black") +
    geom_vline(xintercept = obs_diff, color = "red", linetype = "dashed", size = 1) +
    labs(title = "Permutation Test Distribution",
         x = "Permutation Differences",
         y = "Frequency") +
    theme_minimal()

  return(list(
    Observed_Difference = obs_diff,
    P_Value = p_value,
    Permutation_Distribution_Plot = perm_plot
  ))
}
