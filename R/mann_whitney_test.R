#' Mann-Whitney U Test
#'
#' Performs a Mann-Whitney U test (Wilcoxon Rank-Sum test) to compare two independent groups when data is not normally distributed.
#'
#' @param data A data frame containing UX metrics.
#' @param group_col The column name for the grouping variable (factor with 2 levels).
#' @param value_col The column name for the numeric values to compare.
#'
#' @return A list with Mann-Whitney test results.
#' @export
#'
#' @examples
#' test_data <- data.frame(Group = rep(c("A", "B"), each = 5),
#'                         Score = c(10, 20, 30, 40, 50, 12, 22, 32, 42, 52))
#' mann_whitney_test(test_data, "Group", "Score")
mann_whitney_test <- function(data, group_col, value_col) {
  wilcox.test(data[[value_col]] ~ data[[group_col]], data = data)
}
