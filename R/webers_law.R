#' Weber’s Law Model for UX Sensitivity Analysis
#'
#' Computes the Just Noticeable Difference (JND) based on Weber’s Law.
#'
#' @param stimulus A numeric vector representing the original stimulus intensity (e.g., size, brightness).
#' @param k The Weber fraction, a constant that varies by sense modality (default is 0.1).
#'
#' @return A numeric vector of JND values.
#' @export
#'
#' @examples
#' stimulus <- c(10, 20, 50, 100)
#' webers_law(stimulus)
webers_law <- function(stimulus, k = 0.1) {
  jnd <- k * stimulus
  return(jnd)
}
