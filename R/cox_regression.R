#' Cox Regression Model for Time-to-Completion Analysis in UX Studies
#'
#' Fits a Cox proportional hazards model to analyze time-to-event UX data, such as task completion time or user retention.
#' Provides hazard ratios, confidence intervals, and model diagnostics.
#'
#' @param data A data frame containing UX variables.
#' @param time_col The column name representing time until an event (e.g., task completion time).
#' @param event_col The column name representing event occurrence (1 = event occurred, 0 = censored).
#' @param predictors A formula specifying predictor variables (e.g., `~ experience + device_type`).
#'
#' @return A list containing model summary, hazard ratios, confidence intervals, and proportional hazards test.
#' @export
#'
#' @examples
#' library(survival)
#' test_data <- data.frame(
#'   time = c(5, 10, 15, 20, 25),
#'   event = c(1, 1, 0, 1, 0),
#'   experience = c(1, 2, 3, 4, 5),
#'   device_type = factor(c("Mobile", "Desktop", "Mobile", "Desktop", "Mobile"))
#' )
#' cox_regression(test_data, "time", "event", ~ experience + device_type)

cox_regression <- function(data, time_col, event_col, predictors) {
  if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
  if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")

  library(survival)
  library(broom)

  # Define the Cox regression formula
  formula <- as.formula(paste("Surv(", time_col, ",", event_col, ") ~", deparse(predictors)))

  # Fit the Cox proportional hazards model
  model <- coxph(formula, data = data)

  # Extract hazard ratios and confidence intervals
  hazard_ratios <- exp(coef(model))
  ci_intervals <- exp(confint(model))

  # Test proportional hazards assumption
  proportional_hazards_test <- cox.zph(model)

  return(list(
    Model_Summary = summary(model),
    Hazard_Ratios = hazard_ratios,
    Confidence_Intervals = ci_intervals,
    Proportional_Hazards_Test = tidy(proportional_hazards_test)
  ))
}
