#' GOMS Model for UX Task Performance Prediction
#'
#' Estimates task execution time using the GOMS model.
#'
#' @param keystrokes Number of keystrokes required for the task.
#' @param pointing_actions Number of pointing (mouse movement) actions required.
#' @param mental_operations Number of cognitive (thinking) operations involved.
#' @param system_response Time taken by the system to respond in seconds (default is 0).
#'
#' @return Estimated total task execution time in seconds.
#' @export
#'
#' @examples
#' goms_model(keystrokes = 5, pointing_actions = 3, mental_operations = 2, system_response = 1)
goms_model <- function(keystrokes, pointing_actions, mental_operations, system_response = 0) {
  # Approximate GOMS timings in seconds
  keystroke_time <- 0.2  # Time per keystroke
  pointing_time <- 1.1   # Time per pointing action
  mental_time <- 1.2     # Time per cognitive step

  total_time <- (keystrokes * keystroke_time) +
    (pointing_actions * pointing_time) +
    (mental_operations * mental_time) +
    system_response

  return(total_time)
}
