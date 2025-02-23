#' Chi-Square Test for Categorical Data
#'
#' Performs a chi-square test to determine if there is a significant association between two categorical variables.
#'
#' @param data A data frame containing UX categorical data.
#' @param categorical_col1 The first categorical column name.
#' @param categorical_col2 The second categorical column name.
#'
#' @return A list with Chi-Square test results.
#' @export
#'
#' @examples
#' test_data <- data.frame(UI_Version = rep(c("A", "B"), each = 50),
#'                         Task_Success = sample(c("Success", "Fail"), 100, replace = TRUE))
#' chi_square_test(test_data, "UI_Version", "Task_Success")
chi_square_test <- function(data, categorical_col1, categorical_col2) {
  contingency_table <- table(data[[categorical_col1]], data[[categorical_col2]])
  chisq.test(contingency_table)
}
