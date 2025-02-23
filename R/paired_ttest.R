#' Paired Samples t-Test
#'
#' Compares two related samples (e.g., before and after a UX change) using a paired t-test.
#'
#' @param data A data frame containing UX metrics.
#' @param pre_col The column name for pre-test values.
#' @param post_col The column name for post-test values.
#'
#' @return A list with t-test results.
#' @export
#'
#' @examples
#' test_data <- data.frame(Before = c(55, 60, 65, 70, 75),
#'                         After = c(60, 63, 68, 72, 78))
#' paired_ttest(test_data, "Before", "After")
paired_ttest <- function(data, pre_col, post_col) {
  t.test(data[[pre_col]], data[[post_col]], paired = TRUE)
}
