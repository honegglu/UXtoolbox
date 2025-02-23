#' Hierarchical Clustering for UX Segmentation
#'
#' Performs hierarchical clustering to group users based on behavioral or UX metrics.
#' Provides a dendrogram visualization for cluster interpretation.
#'
#' @param data A data frame containing UX variables (numeric only).
#' @param method The distance metric for clustering (default is "euclidean").
#' @param linkage The linkage method for clustering (default is "complete").
#' @param k The number of clusters to cut the dendrogram into (default is NULL).
#'
#' @return A list containing cluster assignments and a dendrogram plot.
#' @export
#'
#' @examples
#' test_data <- data.frame(task_time = c(5, 10, 15, 20, 25),
#'                         errors = c(1, 2, 1, 3, 2))
#' hierarchical_clustering(test_data)

hierarchical_clustering <- function(data, method = "euclidean", linkage = "complete", k = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("dendextend", quietly = TRUE)) install.packages("dendextend")

  library(ggplot2)
  library(dendextend)

  # Compute distance matrix and hierarchical clustering
  distance_matrix <- dist(data, method = method)
  hc_model <- hclust(distance_matrix, method = linkage)

  # Cut tree into clusters if k is specified
  clusters <- if (!is.null(k)) cutree(hc_model, k = k) else rep(1, nrow(data))

  # Create dendrogram
  dend <- as.dendrogram(hc_model) %>% set("branches_k_color", k = ifelse(is.null(k), 2, k)) %>%
    set("branches_lwd", 2) %>%
    set("labels_cex", 0.7)

  dendrogram_plot <- ggplot() +
    geom_segment(data = as.ggdend(dend), aes(x = x, y = y, xend = xend, yend = yend)) +
    labs(title = "Hierarchical Clustering Dendrogram") +
    theme_minimal()

  return(list(
    Cluster_Assignments = clusters,
    Dendrogram_Plot = dendrogram_plot
  ))
}
