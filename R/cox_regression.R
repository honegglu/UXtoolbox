#' Cox Regression Model for Time-to-Completion Analysis
#'
#' Fits a Cox proportional hazards model to analyze time-to-event UX data.
#'
#' @param data A data frame containing UX variables.
#' @param time_col The column name representing time until an event (e.g., task completion time).
#' @param event_col The column name representing event occurrence (1 = event occurred, 0 = censored).
#' @param predictors A formula specifying predictor variables (e.g., `~ experience + device_type`).
#'
#' @return A summary of the Cox regression model.
#' @export
#'
#' @examples
#' library(survival)
#' test_data <- data.frame(time = c(5, 10, 15, 20, 25),
#'                         event = c(1, 1, 0, 1, 0),
#'                         experience = c(1, 2, 3, 4, 5),
#'                         device_type = factor(c("Mobile", "Desktop", "Mobile", "Desktop", "Mobile")))
#' cox_regression(test_data, "time", "event", ~ experience + device_type)
cox_regression <- function(data, time_col, event_col, predictors) {
  formula <- as.formula(paste("Surv(", time_col, ",", event_col, ") ~", deparse(predictors)))
  model <- survival::coxph(formula, data = data)
  summary(model)
}
