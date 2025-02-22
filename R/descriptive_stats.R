#' Get quick descriptive stats on your dataset
#' @param data A data frame
#' @param cols Vector of column names
#' @return A summary table with mean, median, SD, and range
#' @export
descriptive_stats <- function(data, cols) {
  data %>%
    select(all_of(cols)) %>%
    summarise_all(list(
      mean = mean,
      median = median,
      sd = sd,
      min = min,
      max = max
    ))
}
