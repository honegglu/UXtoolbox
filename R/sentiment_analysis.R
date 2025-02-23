#' Sentiment Analysis for UX Text Feedback
#'
#' Performs sentiment analysis on UX-related textual data using a pre-trained sentiment dictionary.
#' Provides sentiment scores and a sentiment distribution plot.
#'
#' @param text_data A character vector containing user feedback or comments.
#' @param method The sentiment scoring method (default is "bing").
#'
#' @return A list containing sentiment scores and a sentiment distribution plot.
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

  library(tidytext)
  library(dplyr)
  library(ggplot2)

  # Convert text data into a dataframe
  text_df <- data.frame(text = text_data, stringsAsFactors = FALSE)

  # Load sentiment lexicon
  sentiment_lexicon <- get_sentiments(method)

  # Tokenize and score sentiment
  sentiment_scores <- text_df %>%
    unnest_tokens(word, text) %>%
    inner_join(sentiment_lexicon, by = "word") %>%
    count(word, sentiment, sort = TRUE) %>%
    group_by(sentiment) %>%
    summarise(score = sum(n), .groups = "drop")

  # Sentiment distribution plot
  sentiment_plot <- ggplot(sentiment_scores, aes(x = sentiment, y = score, fill = sentiment)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    labs(title = "Sentiment Analysis of UX Feedback",
         x = "Sentiment",
         y = "Score") +
    theme_minimal()

  return(list(
    Sentiment_Scores = sentiment_scores,
    Sentiment_Plot = sentiment_plot
  ))
}
