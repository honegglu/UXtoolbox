#' Hierarchical Clustering for UX Segmentation
#'
#' Performs hierarchical clustering to group users based on behavioral or UX metrics.
#'
#' @param data A data frame containing UX variables (numeric only).
#' @param method The distance metric for clustering (default is "euclidean").
#' @param linkage The linkage method for clustering (default is "complete").
#'
#' @return A hierarchical clustering object.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         errors = c(1, 2, 1, 3, 2))
#' hierarchical_clustering(test_data)
hierarchical_clustering <- function(data, method = "euclidean", linkage = "complete") {
  distance_matrix <- dist(data, method = method)
  model <- hclust(distance_matrix, method = linkage)
  return(model)
}
