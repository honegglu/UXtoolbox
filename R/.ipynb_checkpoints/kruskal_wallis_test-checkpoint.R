#' Kruskal-Wallis Test (Non-Parametric ANOVA) for UX Studies
#'
#' Performs a Kruskal-Wallis test to compare three or more independent groups when data is not normally distributed.
#' Provides effect size (eta-squared) and pairwise post-hoc comparisons.
#'
#' @param data A data frame containing UX metrics.
#' @param group_col The column name for the grouping variable (factor with 3+ levels).
#' @param value_col The column name for the numeric values to compare.
#'
#' @return A list containing the Kruskal-Wallis test results, effect size (eta-squared), and post-hoc pairwise comparisons.
#' @export
#'
#' @examples
#' test_data <- data.frame(
#'   Group = rep(c("A", "B", "C"), each = 5),
#'   Score = c(10, 20, 30, 40, 50, 12, 22, 32, 42, 52, 15, 25, 35, 45, 55)
#' )
#' kruskal_wallis_test(test_data, "Group", "Score")

kruskal_wallis_test <- function(data, group_col, value_col) {
  if (!requireNamespace("rstatix", quietly = TRUE)) install.packages("rstatix")

  library(rstatix)

  # Perform Kruskal-Wallis test
  test_result <- kruskal_test(data, formula(paste(value_col, "~", group_col)))

  # Compute effect size (eta-squared)
  effect_size <- kruskal_effsize(data, formula(paste(value_col, "~", group_col)))

  # Perform post-hoc pairwise comparisons
  pairwise_results <- data %>%
    dunn_test(formula(paste(value_col, "~", group_col)), p.adjust.method = "bonferroni")

  return(list(
    Kruskal_Wallis_Test = test_result,
    Effect_Size = effect_size,
    Pairwise_Comparisons = pairwise_results
  ))
}
