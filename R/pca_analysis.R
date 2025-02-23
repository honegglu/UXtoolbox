#' Principal Component Analysis (PCA) for UX Data Reduction
#'
#' Performs PCA to reduce dimensionality in UX datasets while retaining key variance.
#'
#' @param data A data frame with numeric UX metrics.
#' @param scale Whether to scale the variables before PCA (default is TRUE).
#' @param n_components The number of principal components to retain (default is NULL, keeps all).
#'
#' @return A list containing PCA summary and component loadings.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         errors = c(1, 2, 1, 3, 2),
#'                         clicks = c(20, 25, 22, 30, 28))
#' pca_analysis(test_data)
pca_analysis <- function(data, scale = TRUE, n_components = NULL) {
  pca_model <- prcomp(data, scale. = scale)
  summary_pca <- summary(pca_model)

  if (!is.null(n_components)) {
    pca_model$x <- pca_model$x[, 1:n_components]
  }

  return(list(summary = summary_pca, loadings = pca_model$rotation, scores = pca_model$x))
}
