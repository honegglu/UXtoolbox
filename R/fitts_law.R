#' Fitts' Law Model for UX Performance Prediction
#'
#' Computes movement time using Fitts' Law based on target distance and size, predicting user interaction efficiency.
#' Supports both standard and weighted models for enhanced UX evaluation.
#'
#' @param distance A numeric vector representing the distance to the target (in pixels or cm).
#' @param width A numeric vector representing the target width (in pixels or cm).
#' @param a The regression constant (default is 0.2).
#' @param b The regression coefficient (default is 0.1).
#' @param weighted If TRUE, applies a weighted index for more realistic interaction modeling (default is FALSE).
#'
#' @return A numeric vector of predicted movement times.
#' @export
#'
#' @examples
#' distance <- c(100, 200, 300)
#' width <- c(10, 20, 30)
#' fitts_law(distance, width)
#' fitts_law(distance, width, weighted = TRUE)

fitts_law <- function(distance, width, a = 0.2, b = 0.1, weighted = FALSE) {
  if (weighted) {
    index_of_difficulty <- log2((distance / width) + 1) * 1.1  # Slight weight adjustment for real-world interactions
  } else {
    index_of_difficulty <- log2((distance / width) + 1)
  }

  movement_time <- a + (b * index_of_difficulty)
  return(movement_time)
}
