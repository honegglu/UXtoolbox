#' Factor Analysis for UX Data Exploration
#'
#' Performs Factor Analysis (FA) to identify latent factors in UX datasets.
#'
#' @param data A data frame with numeric UX variables.
#' @param n_factors The number of factors to extract (default is 2).
#' @param rotation The rotation method for factor loadings (default is "varimax").
#'
#' @return A list containing factor loadings and uniqueness scores.
#' @export
#'
#' @examples
#' library(psych)
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         errors = c(1, 2, 1, 3, 2),
#'                         clicks = c(20, 25, 22, 30, 28),
#'                         satisfaction = c(4, 5, 3, 4, 2))
#' factor_analysis(test_data, n_factors = 2)
factor_analysis <- function(data, n_factors = 2, rotation = "varimax") {
  fa_model <- psych::fa(data, nfactors = n_factors, rotate = rotation)
  return(list(loadings = fa_model$loadings, uniqueness = fa_model$uniquenesses))
}
