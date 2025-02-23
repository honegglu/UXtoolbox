#' One-Way ANOVA Test
#'
#' Performs a one-way ANOVA to compare three or more independent groups.
#'
#' @param data A data frame containing UX metrics.
#' @param group_col The column name for the grouping variable (factor with 3+ levels).
#' @param value_col The column name for the numeric values to compare.
#'
#' @return An ANOVA model summary.
#' @export
#'
#' @examples
#' test_data <- data.frame(Group = rep(c("A", "B", "C"), each = 5),
#'                         Score = c(10, 20, 30, 40, 50, 12, 22, 32, 42, 52, 15, 25, 35, 45, 55))
#' anova_test(test_data, "Group", "Score")
anova_test <- function(data, group_col, value_col) {
  model <- aov(data[[value_col]] ~ data[[group_col]], data = data)
  summary(model)
}
