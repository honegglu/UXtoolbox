#' Fitts' Law Model for UX Performance Prediction
#'
#' Computes movement time using Fitts' Law based on target distance and size.
#'
#' @param distance A numeric vector representing the distance to the target (in pixels or cm).
#' @param width A numeric vector representing the target width (in pixels or cm).
#' @param a The regression constant (default is 0.2).
#' @param b The regression coefficient (default is 0.1).
#'
#' @return A numeric vector of predicted movement times.
#' @export
#'
#' @examples
#' distance <- c(100, 200, 300)
#' width <- c(10, 20, 30)
#' fitts_law(distance, width)
fitts_law <- function(distance, width, a = 0.2, b = 0.1) {
  index_of_difficulty <- log2((distance / width) + 1)
  movement_time <- a + (b * index_of_difficulty)
  return(movement_time)
}
