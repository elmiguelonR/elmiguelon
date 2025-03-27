library(httr)
library(jsonlite)
library(tm)
library(text2vec)
library(tidytext)
library(dplyr)
library(future.apply)

plan(multisession)  # Enables parallel execution across CPU cores


#' Fake News Detection: Clickbait & Similarity Analysis
#'
#' This function allows users to choose between clickbait detection and content similarity analysis.
#' Users can also choose between using OpenAI API or R-based NLP analysis.
#'
#' @param news A dataframe containing output from get_news_everything() method
#'
#' @returns A dataframe with the chosen analysis results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get articles
#' news <- get_news_everything(
#'  keyword = "Apple",
#'  from = "2025-03-01",
#'  to = "2025-03-11",
#'  sortBy = "popularity"
#' )
#'
#' # Run function
#' result <- fake_news_detection(news)
#' print(result)
#'
#' }
fake_news_detection <- function(news) {
  model = "gpt-4o-mini"
  # 1) Ask user for analysis type
  cat("Choose the type of fake news detection analysis:\n")
  cat("1: Clickbait Detection\n")
  cat("2: Similarity Analysis\n")
  analysis_choice <- as.integer(readline("Enter 1 or 2: "))
  
  # Check if valid user input
  if (!analysis_choice %in% c(1, 2)) {
    stop("Invalid choice. Please enter 1 or 2.")
  }
  
  # 2) Ask user for method
  cat("\nChoose a Text Analysis Method:\n")
  cat("1: OpenAI Advanced Language Model\n")
  cat("2: R NLP-based Analysis\n")
  method_choice <- as.integer(readline("Enter 1 or 2: "))
  
  # Check if valid user input
  if (!method_choice %in% c(1, 2)) {
    stop("Invalid choice. Please enter 1 or 2.")
  }
  
  # Analysis 1) OpenAI-based Clickbait Detection
  if (analysis_choice == 1 && method_choice == 1) {
    cat("\nPerforming Clickbait Detection using OpenAI...\n")
    
    articles <- news[, c('title'), drop = FALSE]
    
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") stop("OPENAI_API_KEY is not set.")
    
    clickbait_results <- function(title) {
      prompt <- paste0("Is the following headline clickbait? Respond with 'Yes' or 'No'.\n\n", title)
      response <- httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          `Authorization` = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body = jsonlite::toJSON(list(
          model = model,
          messages = list(list(role = "user", content = prompt))
        ), auto_unbox = TRUE)
      )
      
      text <- httr::content(response, "text", encoding = "UTF-8")
      
      result <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)  # Handle invalid JSON
      
      # Check if parsing was successful
      if (is.null(result) || !is.list(result)) {
        return("JSON Parse Error")  # Handle parsing errors
      }
      
      # Check if the response contains an error
      if (!is.null(result) && is.list(result) && "error" %in% names(result)) {
        return(paste("API Error:", result$error$message))  # Handle API errors
      }
      
      # Check if the result is valid and contains choices
      if (!is.null(result) && is.list(result) && "choices" %in% names(result) && length(result$choices) > 0) {
        return(result$choices[[1]]$message$content)
      } else {
        return("Unknown")  # Default response for unexpected API behavior
      }
    }
    
    # Apply function to all titles
    articles$clickbait <- sapply(articles$title, clickbait_results)
    
    # Analysis 2) R-based Clickbait Detection
  } else if (analysis_choice == 1 && method_choice == 2) {
    cat("\nPerforming Clickbait Detection using R NLP...\n")
    
    articles <- news[, c('title'), drop = FALSE]
    
    clickbait_words <- c("shocking", "you won't believe", "unbelievable", "must see",
                         "sensational", "what happens next", "revealed", "secret",
                         "hidden-truth", "little-known", # Curiosity & Intrigue
                         "act now", "hurry", "before it's too late", "don't miss this", "breaking", "right now", # Urgency and Fear of Missing Out
                         "top 10", "best ever", "incredible", "craziest", "mind-blowing", "amazing", "epic", # Superlatives
                         "worst", "disaster", "ruined", "banned", "Outrage" #Fear & Controversy
    )
    
    articles$clickbait <- sapply(articles$title, function(title) {
      if (any(sapply(clickbait_words, function(word) grepl(word, title, ignore.case = TRUE)))) {
        return("Yes")
      } else {
        return("No")
      }
    })
    
    
    # Analysis 3) OpenAI-based Similarity Analysis
  } else if (analysis_choice == 2 && method_choice == 1) {
    
    cat("\nNote: The function retrieves full article content via web scraping. Please wait for the analysis to complete!\n")
    
    # Data pre-processing on 'news' dataframe
    news$full_content <- sapply(news$url, get_full_article)
    articles <- news[,c('title', 'full_content')]
    
    cat("\nPerforming Similarity Analysis using OpenAI...\n")
    cat("\nDue to limitations of the OpenAI API (gpt-4o-mini), analysis is restricted to the first 25 articles!\n")
    
    # Retrieve first 25 articles
    articles <- articles[1:25,]
    
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") stop("OPENAI_API_KEY is not set.")
    
    similarity_matrix <- matrix(0, nrow = nrow(articles), ncol = nrow(articles))
    
    # Function to call OpenAI API for similarity score
    get_similarity <- function(i, j) {
      prompt <- paste0("Compare the following two news articles and rate their similarity from 0 to 1:\n\nArticle 1: ",
                       articles$full_content[i], "\n\nArticle 2: ", articles$full_content[j], "\n\nReturn only a numerical value.")
      response <- httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          `Authorization` = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body = jsonlite::toJSON(list(
          model = model,  # Use GPT-4o Mini
          messages = list(list(role = "user", content = prompt))
        ), auto_unbox = TRUE)
      )
      
      text <- httr::content(response, "text", encoding="UTF-8")
      result <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
      
      if (!is.null(result) && "choices" %in% names(result)) {
        return(as.numeric(result$choices[[1]]$message$content))
      }
      
      return(NA)  # Return NA if API fails
      #Sys.sleep(0.1)
    }
    
    # Generate index pairs for unique comparisons (i < j)
    index_pairs <- expand.grid(i = 1:nrow(articles), j = 1:nrow(articles))
    index_pairs <- subset(index_pairs, i < j)
    
    # Run OpenAI API calls in parallel
    similarity_values <- future.apply::future_mapply(get_similarity, index_pairs$i, index_pairs$j, SIMPLIFY = TRUE)
    
    # Store results in matrix
    for (k in seq_along(similarity_values)) {
      i <- index_pairs$i[k]
      j <- index_pairs$j[k]
      similarity_matrix[i, j] <- similarity_values[k]
      similarity_matrix[j, i] <- similarity_values[k]
    }
    
    # Compute average similarity per row
    avg_similarity_per_row <- apply(similarity_matrix, 1, function(row) {
      valid_scores <- row[row > 0 & !is.na(row)]  # Keep only valid values
      if (length(valid_scores) == 0) return(0)  # Avoid NaN by returning 0 if no valid values
      mean(valid_scores, na.rm = TRUE)  # Ensure NA values are removed from the calculation
    })
    
    # Compute overall similarity score (average of unique pairwise values)
    overall_similarity <- mean(similarity_matrix[lower.tri(similarity_matrix, diag = FALSE)], na.rm = TRUE)
    
    # Assign to dataframe
    articles$similarity_score <- avg_similarity_per_row
    articles$overall_similarity <- overall_similarity
    
    
    # Analysis 4) R-based Similarity Analysis
  } else if (analysis_choice == 2 && method_choice == 2) {
    cat("\nPerforming Similarity Analysis using R NLP...\n")
    
    cat("\nNote: The function retrieves full article content via web scraping. Please wait for the analysis to complete!\n")
    
    # Data pre-processing on 'news' dataframe
    news$full_content <- sapply(news$url, get_full_article)
    articles <- news[,c('title', 'full_content')]
    
    # Clean text
    clean_text <- function(text) {
      text <- tolower(text)
      text <- tm::removePunctuation(text)
      text <- tm::removeNumbers(text)
      text <- tm::removeWords(text, tm::stopwords("en"))
      return(text)
    }
    
    articles$content_clean <- sapply(articles$full_content, clean_text)
    
    # Tokenization and vectorization
    tokens <- text2vec::itoken(articles$content_clean, progressbar = FALSE)
    vectorizer <- text2vec::vocab_vectorizer(text2vec::create_vocabulary(tokens))
    dtm <- text2vec::create_dtm(tokens, vectorizer)
    
    # Compute Cosine Similarity Matrix
    similarity_matrix <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
    
    # Compute One Similarity Value Per Row (Average Similarity with All Other Articles)
    avg_similarity_per_row <- apply(similarity_matrix, 1, function(row) {
      mean(row[-which.max(row)])  # Exclude self-similarity (max value = 1)
    })
    
    # Compute One Similarity Value for Entire Dataset (Overall Average)
    overall_similarity <- mean(similarity_matrix[lower.tri(similarity_matrix)])  # Average of unique pairwise similarities
    
    # Assign to dataframe
    articles$similarity_score <- avg_similarity_per_row  # One value per row
    articles$overall_similarity <- overall_similarity    # One value for dataset (same for all rows)
    
    # Removing the 'content_clean' column
    articles <- subset(articles, select = -content_clean)
  }
  
  return(articles)
}
