#' Principal Component Analysis (PCA) for UX Data Reduction
#'
#' Performs PCA to reduce dimensionality in UX datasets while retaining key variance.
#' Provides a scree plot and biplot for visualization.
#'
#' @param data A data frame with numeric UX metrics.
#' @param scale Whether to scale the variables before PCA (default is TRUE).
#' @param n_components The number of principal components to retain (default is NULL, keeps all).
#'
#' @return A list containing PCA summary, component loadings, scree plot, and biplot.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         errors = c(1, 2, 1, 3, 2),
#'                         clicks = c(20, 25, 22, 30, 28))
#' pca_analysis(test_data)

pca_analysis <- function(data, scale = TRUE, n_components = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")

  library(ggplot2)
  library(factoextra)

  # Perform PCA
  pca_model <- prcomp(data, scale. = scale)
  pca_summary <- summary(pca_model)

  # Extract the required number of components
  if (!is.null(n_components)) {
    pca_model$x <- pca_model$x[, 1:n_components, drop = FALSE]
  }

  # Scree plot
  scree_plot <- fviz_eig(pca_model, addlabels = TRUE) +
    labs(title = "Scree Plot of Principal Components")

  # Biplot
  biplot_plot <- fviz_pca_biplot(pca_model, repel = TRUE) +
    labs(title = "PCA Biplot")

  return(list(
    Summary = pca_summary,
    Loadings = pca_model$rotation,
    Scores = pca_model$x,
    Scree_Plot = scree_plot,
    Biplot = biplot_plot
  ))
}
