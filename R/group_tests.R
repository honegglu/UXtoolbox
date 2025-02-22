#' Compare two groups with a t-test
#' @param data A data frame
#' @param group_col Column name for grouping variable
#' @param value_col Column name for numeric values
#' @return A t-test result
#' @export
compare_groups_ttest <- function(data, group_col, value_col) {
  t.test(
    data[[value_col]] ~ data[[group_col]],
    data = data
  )
}
