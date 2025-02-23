#' Permutation Test for Group Differences
#'
#' Uses a permutation test to compare two independent groups without assuming normality.
#'
#' @param data A data frame containing UX metrics.
#' @param group_col The column name for the grouping variable (factor with 2 levels).
#' @param value_col The column name for the numeric values to compare.
#' @param n_perms The number of permutations to perform (default is 1000).
#'
#' @return A list containing the observed difference, p-value, and permutation distribution.
#' @export
#'
#' @examples
#' test_data <- data.frame(Group = rep(c("A", "B"), each = 10),
#'                         Score = c(10, 12, 15, 18, 20, 25, 30, 35, 40, 45,
#'                                   12, 14, 17, 19, 22, 28, 32, 36, 41, 44))
#' permutation_test(test_data, "Group", "Score", n_perms = 2000)
permutation_test <- function(data, group_col, value_col, n_perms = 1000) {
  obs_diff <- abs(mean(data[[value_col]][data[[group_col]] == unique(data[[group_col]])[1]]) -
                    mean(data[[value_col]][data[[group_col]] == unique(data[[group_col]])[2]]))

  perm_diffs <- replicate(n_perms, {
    shuffled <- sample(data[[value_col]])
    abs(mean(shuffled[1:(nrow(data)/2)]) - mean(shuffled[(nrow(data)/2 + 1):nrow(data)]))
  })

  p_value <- mean(perm_diffs >= obs_diff)

  return(list(observed_difference = obs_diff, p_value = p_value, permutation_distribution = perm_diffs))
}
