#' Time Series Analysis for UX Trends Over Time
#'
#' Performs time series decomposition to detect trends and seasonality in UX data.
#' Provides a decomposition plot for better visualization.
#'
#' @param data A data frame with time and UX metric columns.
#' @param time_col The column name representing time (must be in date format or numeric sequence).
#' @param value_col The column name representing UX metric values.
#' @param frequency The expected frequency of the data (e.g., 12 for monthly data).
#'
#' @return A list containing the decomposed time series object and a visualization plot.
#' @export
#'
#' @examples
#' library(ggplot2)
#' test_data <- data.frame(
#'   date = seq(as.Date("2023-01-01"), by = "month", length.out = 12),
#'   satisfaction_score = c(75, 78, 80, 77, 76, 79, 81, 82, 84, 83, 85, 87)
#' )
#' time_series_analysis(test_data, "date", "satisfaction_score", frequency = 12)

time_series_analysis <- function(data, time_col, value_col, frequency = 12) {
  if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

  library(forecast)
  library(ggplot2)

  # Ensure time column is in date format
  data[[time_col]] <- as.Date(data[[time_col]])

  # Convert to time series object
  ts_data <- ts(data[[value_col]], frequency = frequency,
                start = c(as.numeric(format(min(data[[time_col]]), "%Y")),
                          as.numeric(format(min(data[[time_col]]), "%m"))))

  # Decompose the time series
  decomposed_ts <- decompose(ts_data)

  # Plot decomposition
  decomposed_plot <- autoplot(decomposed_ts) +
    labs(title = "Time Series Decomposition of UX Metric",
         x = "Time",
         y = "Value") +
    theme_minimal()

  return(list(
    Decomposed_Time_Series = decomposed_ts,
    Decomposition_Plot = decomposed_plot
  ))
}
