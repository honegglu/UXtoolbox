#' Independent Samples t-Test
#'
#' Compares two independent groups using a t-test.
#'
#' @param data A data frame.
#' @param group_col The column name for the grouping variable (factor with 2 levels).
#' @param value_col The column name for the numeric values.
#'
#' @return A list with t-test results.
#' @export
#'
#' @examples
#' test_data <- data.frame(Group = rep(c("A", "B"), each = 5),
#'                         Score = c(10, 12, 14, 16, 18, 9, 11, 13, 15, 17))
#' compare_groups_ttest(test_data, "Group", "Score")
compare_groups_ttest <- function(data, group_col, value_col) {
  t.test(
    data[[value_col]] ~ data[[group_col]],
    data = data
  )
}
