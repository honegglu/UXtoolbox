#' One-Way ANOVA for UX and HF Studies
#'
#' Performs a one-way ANOVA to compare three or more independent UX/HF study conditions.
#' Automatically checks assumptions and adjusts for violations.
#'
#' @param data A data frame containing UX metrics.
#' @param group_col The column name for the grouping variable (factor with 3+ levels).
#' @param value_col The column name for the numeric values to compare.
#'
#' @return A list containing ANOVA summary, effect size (η²), assumption checks, and post-hoc results.
#' @export
#'
#' @examples
#' test_data <- data.frame(
#'   Group = rep(c("A", "B", "C"), each = 5),
#'   Score = c(10, 20, 30, 40, 50, 12, 22, 32, 42, 52, 15, 25, 35, 45, 55)
#' )
#' anova_test(test_data, "Group", "Score")

anova_test <- function(data, group_col, value_col) {
  if (!requireNamespace("effectsize", quietly = TRUE)) install.packages("effectsize")
  if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
  if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")

  library(effectsize)
  library(car)
  library(stats)

  data[[group_col]] <- as.factor(data[[group_col]])

  # Assumption checks
  shapiro_test <- shapiro.test(data[[value_col]])  # Normality test
  levene_test <- car::leveneTest(data[[value_col]] ~ data[[group_col]], data = data)  # Homogeneity of variance

  # Decide on standard ANOVA or Welch’s ANOVA based on variance
  if (levene_test$`Pr(>F)`[1] < 0.05) {
    model <- oneway.test(data[[value_col]] ~ data[[group_col]], data = data, var.equal = FALSE)  # Welch's ANOVA
    anova_type <- "Welch's ANOVA (variance not equal)"
  } else {
    model <- aov(data[[value_col]] ~ data[[group_col]], data = data)
    anova_type <- "Standard ANOVA"
  }

  # Calculate Effect Size (Eta-Squared)
  eta_sq <- effectsize::eta_squared(model)

  # Post-hoc test (if standard ANOVA was used)
  if (anova_type == "Standard ANOVA") {
    tukey_results <- TukeyHSD(model)
  } else {
    tukey_results <- "Not applicable (Welch's ANOVA does not use Tukey HSD)"
  }

  # Return results
  return(list(
    ANOVA_Type = anova_type,
    ANOVA_Summary = summary(model),
    Effect_Size_Eta2 = eta_sq,
    Normality_Test = shapiro_test,
    Homogeneity_Test = levene_test,
    PostHoc_Tukey = tukey_results
  ))
}
