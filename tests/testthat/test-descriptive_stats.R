test_that("descriptive_stats works", {
  test_data <- data.frame(A = c(1, 2, 3, 4, 5), B = c(10, 20, 30, 40, 50))
  stats <- descriptive_stats(test_data, c("A", "B"))
  expect_equal(stats$A_mean, mean(test_data$A))
  expect_equal(stats$B_mean, mean(test_data$B))
})
