#' UXtoolbox: A Comprehensive Toolkit for UX and Human Factors Research
#'
#' The UXtoolbox package provides tools for analyzing user experience (UX) data, including Bayesian statistical tests, predictive modeling, usability heuristics, and cognitive modeling.
#'
#' ## Features
#' - Bayesian ANOVA, t-tests, regression, and survival analysis
#' - Mixed-effects and structural equation modeling (SEM)
#' - Implementation of UX laws such as Fitts' Law, Hick's Law, and Weber's Law
#' - Content and sentiment analysis tools
#' - Visualization tools for UX data
#'
#' ## Installation
#' To install the package from GitHub:
#' 
#' ```r
#' install.packages("devtools")  # Install devtools if not already installed
#' devtools::install_github("mohsen-rafiei/UXtoolbox")
#' ```
#'
#' ## Getting Started
#' Load the package:
#' 
#' ```r
#' library(UXtoolbox)
#' ```
#'
#' Perform Bayesian ANOVA:
#' 
#' ```r
#' test_data <- data.frame(
#'   completion_time = c(5.1, 6.3, 5.5, 4.8, 7.2, 6.0, 5.4, 4.9, 7.0, 6.5),
#'   UI_version = factor(rep(c("A", "B", "C"), length.out = 10))
#' )
#' bayesian_anova(test_data, completion_time ~ UI_version)
#' ```
#'
#' ## Package Documentation
#' To view a list of functions in the package, run:
#' 
#' ```r
#' help(package = "UXtoolbox")
#' ```
#'
#' @docType package
#' @name UXtoolbox
NULL
