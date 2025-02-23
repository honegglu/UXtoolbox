#' Content Analysis for UX Text Files
#'
#' Performs content analysis on a text file, including word frequency, sentiment analysis, and topic modeling.
#'
#' @param file_path The path to the text file.
#' @param sentiment_method The sentiment scoring method (default is "bing").
#' @param n_topics The number of topics for topic modeling (default is 3).
#'
#' @return A list containing word frequencies, sentiment scores, and topic modeling results.
#' @export
#'
#' @examples
#' # Assuming "feedback.txt" is a file in your working directory
#' content_analysis("feedback.txt")
content_analysis <- function(file_path, sentiment_method = "bing", n_topics = 3) {
  if (!requireNamespace("tidytext", quietly = TRUE)) install.packages("tidytext")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("tm", quietly = TRUE)) install.packages("tm")
  if (!requireNamespace("topicmodels", quietly = TRUE)) install.packages("topicmodels")
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")

  library(tidytext)
  library(dplyr)
  library(tm)
  library(topicmodels)
  library(readr)

  # Read text file
  text_data <- read_lines(file_path)
  text_df <- data.frame(text = text_data, stringsAsFactors = FALSE)

  # Word frequency analysis
  word_counts <- text_df %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>%
    filter(nchar(word) > 2)  # Filter out short words

  # Sentiment analysis
  sentiment_lexicon <- get_sentiments(sentiment_method)
  sentiment_scores <- text_df %>%
    unnest_tokens(word, text) %>%
    inner_join(sentiment_lexicon, by = "word") %>%
    count(word, sentiment, sort = TRUE) %>%
    group_by(sentiment) %>%
    summarise(score = sum(n), .groups = "drop")

  # Topic modeling
  corpus <- VCorpus(VectorSource(text_df$text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))

  dtm <- DocumentTermMatrix(corpus)
  lda_model <- LDA(dtm, k = n_topics, control = list(seed = 1234))
  topics <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    arrange(topic, desc(beta))

  return(list(word_frequencies = word_counts, sentiment = sentiment_scores, topics = topics))
}
