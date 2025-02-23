#' K-Means Clustering for UX Segmentation
#'
#' Performs K-means clustering to group users based on behavioral or UX metrics.
#' Provides a visualization of cluster assignments.
#'
#' @param data A data frame containing UX variables (numeric only).
#' @param centers The number of clusters to create (default is 3).
#' @param n_start Number of random starts for K-means (default is 25).
#'
#' @return A list containing cluster assignments, cluster centers, and a visualization.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         errors = c(1, 2, 1, 3, 2))
#' kmeans_clustering(test_data, centers = 2)

kmeans_clustering <- function(data, centers = 3, n_start = 25) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")

  library(ggplot2)
  library(stats)

  # Perform K-Means clustering
  model <- kmeans(data, centers = centers, nstart = n_start)

  # Convert to data frame for plotting
  clustered_data <- data.frame(data, Cluster = as.factor(model$cluster))

  # Plot clusters (for 2D visualization, use first two columns)
  cluster_plot <- ggplot(clustered_data, aes(x = clustered_data[, 1], y = clustered_data[, 2], color = Cluster)) +
    geom_point(size = 3) +
    labs(title = "K-Means Clustering Visualization", x = names(data)[1], y = names(data)[2]) +
    theme_minimal()

  return(list(
    Cluster_Assignments = model$cluster,
    Cluster_Centers = model$centers,
    Cluster_Plot = cluster_plot
  ))
}
