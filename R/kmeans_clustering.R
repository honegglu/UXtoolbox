#' K-Means Clustering for UX Segmentation
#'
#' Performs K-means clustering to group users based on behavioral or UX metrics.
#'
#' @param data A data frame containing UX variables (numeric only).
#' @param centers The number of clusters to create (default is 3).
#' @param n_start Number of random starts for K-means (default is 25).
#'
#' @return A list containing cluster assignments and cluster centers.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         errors = c(1, 2, 1, 3, 2))
#' kmeans_clustering(test_data, centers = 2)
kmeans_clustering <- function(data, centers = 3, n_start = 25) {
  model <- kmeans(data, centers = centers, nstart = n_start)
  return(list(clusters = model$cluster, centers = model$centers))
}
