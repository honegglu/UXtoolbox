#' Sentiment Analysis for UX Text Feedback
#'
#' Performs sentiment analysis on UX-related textual data using a pre-trained sentiment dictionary.
#'
#' @param text_data A character vector containing user feedback or comments.
#' @param method The sentiment scoring method (default is "bing").
#'
#' @return A data frame containing sentiment scores for each text entry.
#' @export
#'
#' @examples
#' library(tidytext)
#' library(dplyr)
#' feedback <- c("I love this UI!", "The navigation is terrible.", "It's okay but needs improvements.")
#' sentiment_analysis(feedback)
sentiment_analysis <- function(text_data, method = "bing") {
  if (!requireNamespace("tidytext", quietly = TRUE)) install.packages("tidytext")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

  library(tidytext)
  library(dplyr)

  text_df <- data.frame(text = text_data, stringsAsFactors = FALSE)
  sentiment_lexicon <- get_sentiments(method)

  sentiment_scores <- text_df %>%
    unnest_tokens(word, text) %>%
    inner_join(sentiment_lexicon, by = "word") %>%
    count(word, sentiment, sort = TRUE) %>%
    group_by(sentiment) %>%
    summarise(score = sum(n), .groups = "drop")

  return(sentiment_scores)
}
