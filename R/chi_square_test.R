#' Chi-Square Test for Categorical UX Data
#'
#' Performs a chi-square test to determine if there is a significant association between two categorical UX variables.
#' Provides observed and expected frequencies, effect size (Cramér's V), and test assumptions check.
#'
#' @param data A data frame containing UX categorical data.
#' @param categorical_col1 The first categorical column name.
#' @param categorical_col2 The second categorical column name.
#'
#' @return A list containing Chi-Square test results, effect size (Cramér's V), observed vs. expected counts, and assumption checks.
#' @export
#'
#' @examples
#' test_data <- data.frame(
#'   UI_Version = rep(c("A", "B"), each = 50),
#'   Task_Success = sample(c("Success", "Fail"), 100, replace = TRUE)
#' )
#' chi_square_test(test_data, "UI_Version", "Task_Success")

chi_square_test <- function(data, categorical_col1, categorical_col2) {
  if (!requireNamespace("vcd", quietly = TRUE)) install.packages("vcd")

  library(vcd)

  contingency_table <- table(data[[categorical_col1]], data[[categorical_col2]])

  # Perform Chi-Square test
  test_result <- chisq.test(contingency_table)

  # Compute effect size (Cramér's V)
  cramers_v <- assocstats(contingency_table)$cramer

  # Check assumption: Expected frequency in each cell should be ≥ 5
  min_expected_freq <- min(test_result$expected)
  assumption_check <- ifelse(min_expected_freq >= 5, "Assumption met", "Warning: Some expected frequencies < 5")

  return(list(
    Chi_Square_Test = test_result,
    Cramers_V_Effect_Size = cramers_v,
    Observed_vs_Expected = list(
      Observed = test_result$observed,
      Expected = test_result$expected
    ),
    Assumption_Check = assumption_check
  ))
}
