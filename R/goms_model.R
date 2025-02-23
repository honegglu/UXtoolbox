#' GOMS Model for UX Task Performance Prediction
#'
#' Estimates task execution time using the GOMS model, considering keystrokes, pointing actions, and cognitive operations.
#' Provides a breakdown of task elements and an estimated total execution time.
#'
#' @param keystrokes Number of keystrokes required for the task.
#' @param pointing_actions Number of pointing (mouse movement) actions required.
#' @param mental_operations Number of cognitive (thinking) operations involved.
#' @param system_response Time taken by the system to respond in seconds (default is 0).
#'
#' @return A list containing estimated execution time and a breakdown of the task components.
#' @export
#'
#' @examples
#' goms_model(keystrokes = 5, pointing_actions = 3, mental_operations = 2, system_response = 1)

goms_model <- function(keystrokes, pointing_actions, mental_operations, system_response = 0) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  library(ggplot2)

  # Approximate GOMS timings in seconds
  keystroke_time <- 0.2  # Time per keystroke
  pointing_time <- 1.1   # Time per pointing action
  mental_time <- 1.2     # Time per cognitive step

  # Compute total task execution time
  total_time <- (keystrokes * keystroke_time) +
    (pointing_actions * pointing_time) +
    (mental_operations * mental_time) +
    system_response

  # Create a visualization of task time breakdown
  task_breakdown <- data.frame(
    Component = c("Keystrokes", "Pointing Actions", "Mental Operations", "System Response"),
    Time = c(keystrokes * keystroke_time, pointing_actions * pointing_time, mental_operations * mental_time, system_response)
  )

  breakdown_plot <- ggplot(task_breakdown, aes(x = Component, y = Time, fill = Component)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    labs(title = "GOMS Task Execution Time Breakdown", x = "Task Component", y = "Time (seconds)") +
    theme_minimal()

  return(list(
    Estimated_Total_Time = total_time,
    Task_Time_Breakdown_Plot = breakdown_plot
  ))
}
