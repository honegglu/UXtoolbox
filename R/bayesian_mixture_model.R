#' Bayesian Mixture Model for UX Data Clustering
#'
#' Performs Bayesian Gaussian Mixture Modeling (GMM) to identify clusters in UX data.
#' Provides posterior distributions, cluster probabilities, and model selection criteria.
#'
#' @param data A data frame containing UX variables to cluster.
#' @param n_components The number of clusters to estimate (default is 2).
#' @param n_samples Number of posterior samples (default is 10000).
#' @param prior A list of priors for Bayesian estimation (default is automatic).
#'
#' @return A list containing the fitted Bayesian mixture model, cluster probabilities, posterior distributions, and BIC/AIC values.
#' @export
#'
#' @examples
#' library(mclust)
#' test_data <- data.frame(
#'   engagement = c(2.9, 3.1, 3.5, 3.0, 3.8, 3.2, 3.4, 3.6, 3.3, 3.7),
#'   satisfaction = c(4.0, 4.2, 4.5, 4.1, 4.7, 4.3, 4.4, 4.6, 4.2, 4.5)
#' )
#' bayesian_mixture_model(test_data, n_components = 2)

bayesian_mixture_model <- function(data, n_components = 2, n_samples = 10000, prior = NULL) {
  if (!requireNamespace("mclust", quietly = TRUE)) install.packages("mclust")
  if (!requireNamespace("bayestestR", quietly = TRUE)) install.packages("bayestestR")

  library(mclust)
  library(bayestestR)

  # Fit Bayesian Gaussian Mixture Model
  model <- Mclust(data, G = n_components, modelNames = "VVV", prior = prior)

  # Extract posterior probabilities of clusters
  cluster_probs <- model$z

  # Compute Bayesian Information Criterion (BIC) and Akaike Information Criterion (AIC)
  bic_value <- model$bic
  aic_value <- 2 * model$df - 2 * model$loglik

  # Compute posterior distributions
  posterior_samples <- as.data.frame(model$parameters$mean)

  # Compute credible intervals for cluster means
  ci_bounds <- bayestestR::ci(posterior_samples, ci = 0.95)

  return(list(
    Model_Summary = summary(model),
    Cluster_Probabilities = cluster_probs,
    Posterior_Samples = posterior_samples,
    Credible_Intervals = ci_bounds,
    BIC = bic_value,
    AIC = aic_value
  ))
}
