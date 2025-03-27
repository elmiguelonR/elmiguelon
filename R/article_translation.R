#' Get OpenAI API Response
#'
#' This function sends a request to the OpenAI Chat Completions endpoint
#' using the specified prompt and parameters. It returns the response message content.
#'
#' @param messages A character string containing a customed conversation.
#' @param user_msg A character string containing the user's prompt.
#' @param system_msg A character string containing the system's prompt.
#' @param model A character string specifying the model to use (default: "gpt-3.5-turbo").
#' @param temperature A float meaning randomness.
#'
#' @return A character string with the content of the response message.
#'
get_openai_response <- function(
    messages = NULL,
    user_msg = NULL,
    system_msg = "You are a professional news assistant. Provide exactly what the user requests without any extra commentary.",
    model = "gpt-3.5-turbo",
    temperature = 0.2) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY is not set.")

  # If no custom 'messages' list is provided, build a simple 2-message conversation
  if (is.null(messages)) {
    if (is.null(user_msg)) {
      stop("Either provide 'messages' or 'user_msg' for the user content.")
    }
    messages <- list(
      list(role = "system", content = system_msg),
      list(role = "user", content = user_msg)
    )
  }

  # Construct request body
  body_list <- list(
    model = model,
    temperature = temperature,
    messages = messages
  )

  # Make the POST request
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type`  = "application/json"
    ),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE)
  )

  if (httr::status_code(response) != 200) {
    stop("Request failed with status: ", httr::status_code(response))
  }

  result <- httr::content(response, as = "parsed", type = "application/json")

  # Return the text of the first choice
  result$choices[[1]]$message$content
}


#' Translate the summary of a single article.
#'
#' @param article_row A single row from a data frame containing news articles.
#' @param target_language The language code to translate into (e.g., "ko", "fr"). Default is "en".
#'
#' @return A character string with the translated text.
#'
translate_news_article <- function(article_row, target_language = NULL) {
  title <- if (!is.null(article_row$title)) article_row$title else ""
  url <- if (!is.null(article_row$url)) article_row$url else ""

  content <- get_full_article(url)

  system_msg <- paste0(
    "You are a professional news translation assistant. Your task is to: ",
    "1) Translate the article title accurately into ", target_language, ". ",
    "2) Read the full content of the article. ",
    "3) Create a concise summary (maximum 250 words) of the article in ", target_language, " that captures the main points and key information. ",
    "The summary should maintain the professional tone of news reporting while omitting unnecessary details."
  )

  user_msg <- paste0(
    "Translate the following news article's title and content into ", target_language, ":\n\n",
    "Title: ", title, "\n\n",
    "Content: ", content, "\n"
  )

  translation <- get_openai_response(system_msg = system_msg, user_msg = user_msg)
  translation
}


# ------------------------------------------------------
# Search → Display articles → Choose → Translate
# ------------------------------------------------------

#' Search and Translate the summary of a single article.
#'
#' This function retrieves news articles either from "everything" or "top-headlines,"
#' displays them, prompts the user to choose one by index, and translates it via OpenAI.
#'
#' @param search_method One of "everything" or "top-headlines" (default: "everything").
#' @param keyword Keyword to pass to `get_news_everything()` (required if `search_method = "everything"`).
#' @param from Start date (YYYY-MM-DD).
#' @param to End date (YYYY-MM-DD).
#' @param sortBy Sort order for `get_news_everything()` (e.g., "relevancy", "popularity").
#' @param category Category to pass to `get_top_headlines()` (e.g., "business", "technology").
#'
#' @return The translated text (invisible). Also prints the translated text to the console.
#' @examples
#' \dontrun{
#' # 1) Retrieve articles about "Apple" from March 1 to March 8, 2025 -> choose one -> translate to Korean:
#' search_and_translate_news(
#'   search_method = "everything",
#'   keyword = "Apple",
#'   from = "2025-03-01",
#'   to = "2025-03-08",
#'   sortBy = "relevancy"
#' )
#'
#' # 2) Retrieve top US headlines (business category) -> choose one -> translate to English:
#' search_and_translate_news(
#'   search_method = "top-headlines",
#'   category = "business"
#' )
#' }
#' @export
search_and_translate_news <- function(
    search_method = c("everything", "top-headlines"),
    keyword = NULL,
    from = NULL,
    to = NULL,
    sortBy = "relevancy",
    category = NULL) {
  search_method <- match.arg(search_method)

  # 1) Retrieve news articles based on the chosen method
  if (search_method == "everything") {
    if (is.null(keyword)) {
      stop("You must provide a 'keyword' when search_method = 'everything'.")
    }
    articles <- get_news_everything(
      keyword = keyword,
      from = from,
      to = to,
      sortBy = sortBy
    )
  } else {
    # top-headlines
    articles <- get_top_headlines(category = category)
  }

  if (nrow(articles) == 0) {
    stop("No articles found.")
  }

  # 2) Display a summarized list of retrieved articles (title, source, date/time)
  cat("==== Retrieved Articles ====\n")
  max_len = min(nrow(articles), 20)
  for (i in seq_len(max_len)) {
    cat("[", i, "] ",
        if (!is.null(articles$title[i])) articles$title[i] else "(No title)",
        "\n    Source:", if (!is.null(articles$source.name[i])) articles$source.name[i] else "(No source)",
        "\n    PublishedAt:", if (!is.null(articles$publishedAt[i])) articles$publishedAt[i] else "",
        "\n\n",
        sep = ""
    )
  }

  # 3) Prompt the user to select an article by index
  index_input <- readline(prompt = "Enter the article number to translate: ")
  index_input <- as.integer(index_input)

  if (is.na(index_input) || index_input < 1 || index_input > nrow(articles)) {
    stop("Invalid article number.")
  }

  selected_article <- articles[index_input, ]

  # 4) Translate the selected article
  target_language <- readline(prompt = "Enter the preferred language: ")

  cat("\nTranslating the article using OpenAI...\n")
  translation <- translate_news_article(selected_article, target_language = target_language)

  # 5) Print and return
  translation <- stringi::stri_unescape_unicode(translation)
  cat("\n==== Translation Result ====\n", translation, "\n", sep = "")
}
