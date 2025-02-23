#' Descriptive Statistics Summary
#'
#' Computes mean, median, standard deviation, min, and max for selected columns.
#'
#' @param data A data frame containing UX metrics.
#' @param cols A vector of column names to summarize.
#'
#' @return A data frame with descriptive statistics.
#' @export
#'
#' @examples
#' test_data <- data.frame(A = c(1, 2, 3, 4, 5), B = c(10, 20, 30, 40, 50))
#' descriptive_stats(test_data, c("A", "B"))
descriptive_stats <- function(data, cols) {
  data %>%
    select(all_of(cols)) %>%
    summarise_all(list(
      mean = mean,
      median = median,
      sd = sd,
      min = min,
      max = max
    ), na.rm = TRUE)
}
