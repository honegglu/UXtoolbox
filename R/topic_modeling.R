#' Topic Modeling for UX Feedback Analysis
#'
#' Performs Latent Dirichlet Allocation (LDA) to uncover topics in UX-related textual data.
#' Provides a word distribution plot for each topic.
#'
#' @param text_data A character vector containing user feedback or comments.
#' @param n_topics The number of topics to extract (default is 3).
#' @param n_words The number of top words to return per topic (default is 5).
#'
#' @return A list containing topic-term distributions and a visualization of top words per topic.
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

  library(topicmodels)
  library(tidytext)
  library(dplyr)
  library(tm)
  library(ggplot2)

  # Convert text data into a dataframe
  text_df <- data.frame(text = text_data, stringsAsFactors = FALSE)

  # Preprocess text data
  corpus <- VCorpus(VectorSource(text_df$text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)

  dtm <- DocumentTermMatrix(corpus)

  # Fit LDA model
  lda_model <- LDA(dtm, k = n_topics, control = list(seed = 1234))
  topics <- tidy(lda_model, matrix = "beta")

  # Extract top words per topic
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(n_words, beta) %>%
    arrange(topic, desc(beta))

  # Create topic visualization
  topic_plot <- ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free", ncol = 2) +
    coord_flip() +
    scale_x_reordered() +
    labs(title = "Top Words per Topic",
         x = "Words",
         y = "Probability") +
    theme_minimal()

  return(list(
    Topic_Distribution = top_terms,
    Topic_Plot = topic_plot
  ))
}
