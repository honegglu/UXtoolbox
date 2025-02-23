#' Weber’s Law Model for UX Sensitivity Analysis
#'
#' Computes the Just Noticeable Difference (JND) based on Weber’s Law.
#' Provides a visualization of how JND changes with stimulus intensity.
#'
#' @param stimulus A numeric vector representing the original stimulus intensity (e.g., size, brightness).
#' @param k The Weber fraction, a constant that varies by sense modality (default is 0.1).
#'
#' @return A list containing the computed JND values and a visualization.
#' @export
#'
#' @examples
#' stimulus <- c(10, 20, 50, 100)
#' webers_law(stimulus)

webers_law <- function(stimulus, k = 0.1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

  library(ggplot2)

  # Compute Just Noticeable Difference (JND)
  jnd <- k * stimulus

  # Create a visualization
  jnd_plot <- ggplot(data.frame(stimulus, jnd), aes(x = stimulus, y = jnd)) +
    geom_point(color = "blue", size = 3) +
    geom_line(color = "blue", linetype = "dashed") +
    labs(title = "Weber’s Law: Just Noticeable Difference",
         x = "Stimulus Intensity",
         y = "Just Noticeable Difference (JND)") +
    theme_minimal()

  return(list(
    JND_Values = jnd,
    JND_Plot = jnd_plot
  ))
}
