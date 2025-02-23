#' Descriptive Statistics Summary for UX Metrics
#'
#' Computes detailed descriptive statistics including mean, median, standard deviation, variance, IQR, min, max, and skewness for selected UX-related columns.
#'
#' @param data A data frame containing UX metrics.
#' @param cols A vector of column names to summarize.
#'
#' @return A data frame with comprehensive descriptive statistics.
#' @export
#'
#' @examples
#' test_data <- data.frame(A = c(1, 2, 3, 4, 5), B = c(10, 20, 30, 40, 50))
#' descriptive_stats(test_data, c("A", "B"))

descriptive_stats <- function(data, cols) {
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("moments", quietly = TRUE)) install.packages("moments")

  library(dplyr)
  library(moments)

  data %>%
    select(all_of(cols)) %>%
    summarise_all(list(
      mean = mean,
      median = median,
      sd = sd,
      var = var,
      iqr = IQR,
      min = min,
      max = max,
      skewness = skewness
    ), na.rm = TRUE)
}
