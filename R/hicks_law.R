#' Hick’s Law Model for UX Decision Time Prediction
#'
#' Computes decision time using Hick’s Law based on the number of choices.
#' Provides a visualization of decision time growth as choices increase.
#'
#' @param choices A numeric vector representing the number of available choices.
#' @param a The regression constant (default is 0.2).
#' @param b The regression coefficient (default is 0.1).
#'
#' @return A list containing computed decision times and a visualization.
#' @export
#'
#' @examples
#' choices <- c(2, 4, 8, 16)
#' hicks_law(choices)

hicks_law <- function(choices, a = 0.2, b = 0.1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  library(ggplot2)

  # Compute decision time
  decision_time <- a + (b * log2(choices + 1))

  # Create visualization
  decision_plot <- ggplot(data.frame(Choices = choices, Decision_Time = decision_time), aes(x = Choices, y = Decision_Time)) +
    geom_point(size = 3, color = "blue") +
    geom_line(color = "red", linetype = "dashed") +
    labs(title = "Hick’s Law: Decision Time vs. Choices",
         x = "Number of Choices",
         y = "Decision Time (seconds)") +
    theme_minimal()

  return(list(
    Decision_Time_Values = decision_time,
    Decision_Time_Plot = decision_plot
  ))
}
