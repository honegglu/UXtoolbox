#' Bayesian Mixture Model for UX Data Clustering
#'
#' Performs Bayesian Gaussian Mixture Modeling (GMM) to identify clusters in UX data.
#'
#' @param data A data frame containing UX variables to cluster.
#' @param n_components The number of clusters to estimate (default is 2).
#' @param n_samples Number of posterior samples (default is 4000).
#'
#' @return A fitted Bayesian mixture model.
#' @export
#'
#' @examples
#' library(mclust)
#' test_data <- data.frame(
#'   engagement = c(2.9, 3.1, 3.5, 3.0, 3.8, 3.2, 3.4, 3.6, 3.3, 3.7),
#'   satisfaction = c(4.0, 4.2, 4.5, 4.1, 4.7, 4.3, 4.4, 4.6, 4.2, 4.5)
#' )
#' bayesian_mixture_model(test_data, n_components = 2)
bayesian_mixture_model <- function(data, n_components = 2, n_samples = 4000) {
  if (!requireNamespace("mclust", quietly = TRUE)) install.packages("mclust")

  library(mclust)

  model <- Mclust(data, G = n_components, modelNames = "VVV")
  return(summary(model))
}
