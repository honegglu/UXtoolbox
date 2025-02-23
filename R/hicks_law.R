#' Hick’s Law Model for UX Decision Time Prediction
#'
#' Computes decision time using Hick’s Law based on the number of choices.
#'
#' @param choices A numeric vector representing the number of available choices.
#' @param a The regression constant (default is 0.2).
#' @param b The regression coefficient (default is 0.1).
#'
#' @return A numeric vector of predicted decision times.
#' @export
#'
#' @examples
#' choices <- c(2, 4, 8, 16)
#' hicks_law(choices)
hicks_law <- function(choices, a = 0.2, b = 0.1) {
  decision_time <- a + (b * log2(choices + 1))
  return(decision_time)
}
