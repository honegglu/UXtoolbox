#' Visualize success rates with a bar chart
#' @param data A data frame
#' @param group_col Column name for groups (e.g., UI version)
#' @param success_col Column name for success rate
#' @return A ggplot bar chart
#' @export
plot_success_rates <- function(data, group_col, success_col) {
  ggplot(data, aes_string(x = group_col, y = success_col, fill = group_col)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Success Rate by Condition",
         x = "Condition",
         y = "Success Rate (%)") +
    theme_minimal()
}
