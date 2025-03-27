# SentimentAnalysis.R
# This script implements sentiment analysis using both the OpenAI API and native R methods.
# It also includes plotting functions to visualize sentiment distribution, sentiment evolution over time,
# and a word cloud of the text data.
# It is assumed that the API keys have already been configured (e.g., using save_api_keys() defined in api.R).

# Load required libraries
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(tibble)
library(magrittr)

#' Get Sentiment using OpenAI API (Detailed)
#'
#' This function performs sentiment analysis by sending a prompt to the OpenAI API.
#' It returns a list with three fields: 'score' (a numeric value between -1 and 1),
#' 'label' (one of: "Positive", "Negative", or "Neutral"), and 'details' (a detailed description of the nuanced emotions).
#'
#' @param text A character string containing the text to analyze.
#' @param model A character string specifying the OpenAI model to use. Default is "gpt-4o-mini".
#' @param role A character string indicating the role of the prompt sender. Default is "user".
#'
#' @return A list with three elements: 'score', 'label', and 'details'.
#'
#' @examples
#' \dontrun{
#' sentiment <- get_sentiment_openai("I am very happy with the results!")
#' print(sentiment)
#' }
#'
#' @export
get_sentiment_openai <- function(text, model = "gpt-4o-mini", role = "user") {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY is not set.")

  prompt <- paste0(
    "Analyze the sentiment of the following text and provide a detailed emotional analysis. ",
    "Return the result as a valid JSON object with exactly three fields: 'score', 'label', and 'details'. ",
    "The 'score' field must be a numeric value between -1 and 1. ",
    "The 'label' field must be one of: 'Positive', 'Negative', or 'Neutral'. ",
    "The 'details' field must be a detailed description of the nuanced emotions present (for example: 'joyful', 'anxious', 'melancholic', etc.). ",
    "Do not include any additional text or markdown formatting. ",
    "Text: ", text, "\n",
    "Return only the JSON."
  )

  messages <- list(list(role = role, content = prompt))
  body_list <- list(
    model = model,
    messages = messages
  )

  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE)
  )

  if (httr::status_code(response) != 200) {
    stop("Request failed with status: ", httr::status_code(response))
  }

  result <- httr::content(response, as = "parsed", type = "application/json")
  answer_text <- result$choices[[1]]$message$content

  # Clean the response to remove markdown formatting (e.g., ```json)
  answer_text <- gsub("^```\\s*json", "", answer_text, ignore.case = TRUE)
  answer_text <- gsub("```", "", answer_text)
  answer_text <- trimws(answer_text)

  sentiment <- tryCatch({
    jsonlite::fromJSON(answer_text)
  }, error = function(e) {
    stop("Error parsing JSON response: ", e$message)
  })

  if (!("score" %in% names(sentiment)) ||
      !("label" %in% names(sentiment)) ||
      !("details" %in% names(sentiment))) {
    stop("The JSON response does not contain the required fields: 'score', 'label', and 'details'.")
  }

  return(sentiment)
}

#' Get Sentiment using Native R Methods
#'
#' This function performs sentiment analysis using native R methods.
#' It tokenizes the input text and calculates a normalized sentiment score based on the 'bing' lexicon.
#' It returns a list with a normalized 'score' and a 'label' ("Positive", "Negative", or "Neutral").
#'
#' @param text A character string containing the text to analyze.
#'
#' @return A list with two elements: 'score' and 'label'.
#'
#' @examples
#' sentiment <- get_sentiment_r("I love sunny days!")
#' print(sentiment)
#'
#' @export
get_sentiment_r <- function(text) {
  df <- tibble::tibble(text = text)
  df_tokens <- df %>% tidytext::unnest_tokens(word, text)
  bing <- tidytext::get_sentiments("bing")
  sentiment_score <- df_tokens %>%
    dplyr::inner_join(bing, by = "word") %>%
    dplyr::mutate(score = ifelse(sentiment == "positive", 1, -1)) %>%
    dplyr::summarise(total = sum(score)) %>%
    dplyr::pull(total)
  total_words <- nrow(df_tokens)
  normalized_score <- if (total_words > 0) sentiment_score / total_words else 0
  label <- if (normalized_score > 0.05) {
    "Positive"
  } else if (normalized_score < -0.05) {
    "Negative"
  } else {
    "Neutral"
  }
  return(list(score = normalized_score, label = label))
}

#' General Sentiment Analysis Function
#'
#' This function selects the sentiment analysis method to use.
#' For the OpenAI method, it always returns the detailed output (including the 'details' field).
#'
#' @param text A character string containing the text to analyze.
#' @param method A character string specifying which method to use ("openai" or "r").
#'
#' @return A list with sentiment analysis results.
#' @importFrom magrittr %>%
#' @examples
#' sentiment <- get_sentiment("I am not sure about this", method = "r")
#' print(sentiment)
#'
#' @export
get_sentiment <- function(text, method = c("openai", "r")) {
  method <- match.arg(method)
  if (method == "openai") {
    return(get_sentiment_openai(text))
  } else {
    return(get_sentiment_r(text))
  }
}

#' Analyze Sentiment of a Data Frame of Articles
#'
#' This function applies sentiment analysis to each article in a data frame.
#' It adds two new columns: 'sentiment_score' and 'sentiment_label'. For the OpenAI method,
#' it always forces the detailed output (with the 'details' field) by using the detailed prompt.
#' The function includes error handling and rate limit management with exponential backoff.
#' If an article fails to be analyzed after the maximum number of retries (default 5), the code stops.
#'
#' @param articles A data frame containing the articles.
#' @param text_column A character string indicating the name of the column that contains the text to analyze (default: "content").
#' @param method A character string specifying which method to use ("openai" or "r").
#' @param delay A numeric value specifying the base delay (in seconds) between API calls (default: 2).
#' @param max_retries An integer indicating the maximum number of retries for rate limit errors (default: 5).
#'
#' @return The input data frame with added columns: 'sentiment_score' and 'sentiment_label'.
#'
#' @examples
#' \dontrun{
#' articles <- data.frame(
#'   content = c("Great progress in the project!", "This is terrible news.", "The event was okay."),
#'   publishedAt = c("2025-03-01T12:00:00Z", "2025-03-02T15:30:00Z", "2025-03-03T09:45:00Z"),
#'   stringsAsFactors = FALSE
#' )
#' analyzed_articles <- analyze_sentiment(articles, text_column = "content", method = "r")
#' head(analyzed_articles)
#' }
#'
#' @export
analyze_sentiment <- function(articles, text_column = "content", method = c("openai", "r"), delay = 2, max_retries = 5) {
  method <- match.arg(method)

  articles$sentiment_score <- NA
  articles$sentiment_label <- NA

  for (i in 1:nrow(articles)) {
    cat("Analyzing article", i, "of", nrow(articles), "...\n")
    text <- articles[[text_column]][i]

    retries <- 0
    success <- FALSE
    sentiment <- list(score = NA, label = NA)

    while (!success && retries < max_retries) {
      sentiment <- tryCatch({
        get_sentiment(text, method = method)
      }, error = function(e) {
        if (grepl("429", e$message)) {
          wait_time <- delay * 2^(retries)
          message("Rate limit hit on article ", i, ". Retrying in ", wait_time, " seconds...")
          Sys.sleep(wait_time)
          return(NULL)
        } else {
          stop("Error analyzing article ", i, ": ", e$message)
        }
      })

      if (!is.null(sentiment)) {
        success <- TRUE
      } else {
        retries <- retries + 1
      }
    }

    if (!success) {
      stop("Failed to analyze article ", i, " after ", max_retries, " retries.")
    }

    articles$sentiment_score[i] <- sentiment$score
    articles$sentiment_label[i] <- sentiment$label

    Sys.sleep(delay)
  }

  return(articles)
}

#' Plot Sentiment Distribution
#'
#' This function generates a bar plot to visualize the sentiment distribution (based on sentiment labels).
#'
#' @param articles A data frame containing the articles with sentiment analysis results.
#' @param label_column A character string indicating the column name for sentiment labels (default: "sentiment_label").
#'
#' @return None. The function prints the plot.
#'
#' @examples
#' \dontrun{
#' plot_sentiment_distribution(analyzed_articles, label_column = "sentiment_label")
#' }
#'
#' @export
plot_sentiment_distribution <- function(articles, label_column = "sentiment_label") {
  p1 <- ggplot2::ggplot(articles, ggplot2::aes_string(x = label_column)) +
    ggplot2::geom_bar(fill = "skyblue") +
    ggplot2::labs(title = "Sentiment Distribution", x = "Sentiment Label", y = "Count") +
    ggplot2::theme_minimal()
  suppressWarnings(print(p1))
}

#' Plot Sentiment Evolution Over Time
#'
#' This function groups articles by date (based on the 'publishedAt' column) and plots the evolution
#' of the average sentiment score over time.
#'
#' @param articles A data frame containing the articles with sentiment analysis results.
#' @param date_column A character string indicating the column name for publication dates (default: "publishedAt").
#' @param score_column A character string indicating the column name for sentiment scores (default: "sentiment_score").
#'
#' @return None. The function prints the plot.
#'
#' @examples
#' \dontrun{
#' plot_sentiment_over_time(analyzed_articles, date_column = "publishedAt", score_column = "sentiment_score")
#' }
#'
#' @export
plot_sentiment_over_time <- function(articles, date_column = "publishedAt", score_column = "sentiment_score") {
  articles <- articles %>% dplyr::mutate(date = as.Date(lubridate::ymd_hms(.data[[date_column]], quiet = TRUE)))
  if (all(is.na(articles$date))) {
    warning("No valid dates found in the provided date column.")
    return(NULL)
  }
  daily_sentiment <- articles %>% dplyr::group_by(date) %>% dplyr::summarise(avg_score = mean(.data[[score_column]], na.rm = TRUE))
  p <- ggplot2::ggplot(daily_sentiment, ggplot2::aes(x = date, y = avg_score)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(color = "red") +
    ggplot2::labs(title = "Sentiment Evolution Over Time", x = "Date", y = "Average Sentiment Score") +
    ggplot2::theme_minimal()
  print(p)
}

#' Plot Word Cloud
#'
#' This function generates a word cloud from the text column in the articles data frame.
#' It tokenizes the text, removes stopwords, calculates word frequencies, and plots the word cloud.
#'
#' @param articles A data frame containing the articles.
#' @param text_column A character string indicating the column name that contains the text (default: "content").
#' @param max_words An integer specifying the maximum number of words to display in the word cloud (default: 100).
#'
#' @return None. The function prints the word cloud.
#'
#' @examples
#' \dontrun{
#' plot_word_cloud(analyzed_articles, text_column = "content", max_words = 50)
#' }
#'
#' @export
plot_word_cloud <- function(articles, text_column = "content", max_words = 100) {
  if (!require(wordcloud, quietly = TRUE)) {
    stop("The 'wordcloud' package is required but not installed. Please install it using install.packages('wordcloud').")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("The 'RColorBrewer' package is required but not installed. Please install it using install.packages('RColorBrewer').")
  }

  df <- articles %>% dplyr::select(!!rlang::sym(text_column)) %>% tidytext::unnest_tokens(word, !!rlang::sym(text_column))
  data("stop_words", package = "tidytext")
  df <- df %>% dplyr::anti_join(stop_words, by = "word")
  word_freq <- df %>% dplyr::count(word, sort = TRUE)

  suppressWarnings(
    wordcloud::wordcloud(
      words = word_freq$word,
      freq = word_freq$n,
      max.words = max_words,
      colors = RColorBrewer::brewer.pal(8, "Dark2")
    )
  )
}

#' Interactive Plot Selection
#'
#' This function prompts the user to select which plot to display:
#' 1: Sentiment Distribution (bar plot for sentiment labels)
#' 2: Sentiment Evolution Over Time (line plot)
#' 3: Word Cloud
#' 4: All Plots
#'
#' @param articles A data frame containing the articles with sentiment analysis results.
#' @param date_column A character string for the publication date column (default: "publishedAt").
#' @param score_column A character string for the sentiment score column (default: "sentiment_score").
#' @param label_column A character string for the sentiment label column (default: "sentiment_label").
#' @param text_column A character string for the text column (default: "content").
#'
#' @return None. The function displays the selected plot(s).
#'
#' @examples
#' \dontrun{
#' interactive_plot(analyzed_articles, date_column = "publishedAt",
#'                  score_column = "sentiment_score", label_column = "sentiment_label", text_column = "content")
#' }
#'
#' @export
interactive_plot <- function(articles, date_column = "publishedAt", score_column = "sentiment_score", label_column = "sentiment_label", text_column = "content") {
  cat("Select which plot to display:\n")
  cat("1: Sentiment Distribution (bar plot for sentiment labels)\n")
  cat("2: Sentiment Evolution Over Time (line plot)\n")
  cat("3: Word Cloud\n")
  cat("4: All Plots\n")
  choice <- as.integer(readline(prompt = "Enter your choice (1-4): "))

  if(choice == 1) {
    plot_sentiment_distribution(articles, label_column = label_column)
  } else if (choice == 2) {
    plot_sentiment_over_time(articles, date_column = date_column, score_column = score_column)
  } else if (choice == 3) {
    plot_word_cloud(articles, text_column = text_column)
  } else if (choice == 4) {
    plot_sentiment_distribution(articles, label_column = label_column)
    plot_sentiment_over_time(articles, date_column = date_column, score_column = score_column)
    plot_word_cloud(articles, text_column = text_column)
  } else {
    cat("Invalid choice. No plot will be displayed.\n")
  }
}
