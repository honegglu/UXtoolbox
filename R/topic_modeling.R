#' Topic Modeling for UX Feedback Analysis
#'
#' Performs Latent Dirichlet Allocation (LDA) to uncover topics in UX-related textual data.
#'
#' @param text_data A character vector containing user feedback or comments.
#' @param n_topics The number of topics to extract (default is 3).
#' @param n_words The number of top words to return per topic (default is 5).
#'
#' @return A list of topics with their top words.
#' @export
#'
#' @examples
#' library(topicmodels)
#' library(tidytext)
#' library(dplyr)
#' feedback <- c("The interface is very intuitive and easy to use.",
#'               "I had trouble navigating the settings menu.",
#'               "The app crashes frequently on my phone.",
#'               "I love the new design, it's very modern and clean.",
#'               "The checkout process is confusing.")
#' topic_modeling(feedback, n_topics = 2)
topic_modeling <- function(text_data, n_topics = 3, n_words = 5) {
  if (!requireNamespace("topicmodels", quietly = TRUE)) install.packages("topicmodels")
  if (!requireNamespace("tidytext", quietly = TRUE)) install.packages("tidytext")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("tm", quietly = TRUE)) install.packages("tm")

  library(topicmodels)
  library(tidytext)
  library(dplyr)
  library(tm)

  text_df <- data.frame(text = text_data, stringsAsFactors = FALSE)

  # Preprocess text data
  corpus <- VCorpus(VectorSource(text_df$text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))

  dtm <- DocumentTermMatrix(corpus)

  # Fit LDA model
  lda_model <- LDA(dtm, k = n_topics, control = list(seed = 1234))
  topics <- tidy(lda_model, matrix = "beta")

  # Extract top words per topic
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(n_words, beta) %>%
    arrange(topic, desc(beta))

  return(top_terms)
}
