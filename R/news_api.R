#' Get News Articles from NewsAPI "Everything" Endpoint
#'
#' This function queries the NewsAPI "everything" endpoint using the specified keyword,
#' date range, sort criteria, and language. It returns the retrieved articles as a data frame.
#'
#' @param keyword A character string specifying the search term to query for news articles.
#'   The value will be URL-encoded, and its encoded version must not exceed 500 characters.
#' @param from A character string representing the start date for the news articles in YYYY-MM-DD format.
#'   If NULL, this parameter is omitted from the query.
#' @param to A character string representing the end date for the news articles in YYYY-MM-DD format.
#'   If NULL, this parameter is omitted from the query.
#' @param sortBy A character string to specify the sort order of the articles (e.g., "popularity",
#'   "relevancy", "publishedAt"). If NULL, this parameter is omitted from the query.
#' @param language A character string representing the language of the news articles (default is "en").
#'
#' @return A data frame containing the news articles.
#'
#' @examples
#' \dontrun{
#' # Basic usage - search for "climate" articles in English
#' climate_news <- get_news_everything(keyword = "climate")
#'
#' # Search with date range - articles from the last month
#' ai_news <- get_news_everything(
#'   keyword = "AI",
#'   from = "2025-03-12",
#'   to = "2025-03-16",
#'   sortBy = "publishedAt"
#' )
#'
#' # Search in a different language (Spanish)
#' spanish_news <- get_news_everything(
#'   keyword = "elecciones",
#'   language = "es",
#'   sortBy = "popularity"
#' )
#' # View the first few results
#' head(climate_news)
#' }
#'
#' @export
get_news_everything <- function(keyword = NULL, from = NULL, to = NULL, sortBy = NULL, language = "en") {
  news_url <- "https://newsapi.org/v2/everything"
  api_key <- Sys.getenv("NEWS_API_KEY")
  if (api_key == "") stop("NEWS_API_KEY is not set.")

  # Formatting keyword in URLencode
  keyword_encoded <- URLencode(keyword, reserved = TRUE)
  if (nchar(keyword_encoded) > 500) {
    stop("The URL-encoded query exceeds the maximum length of 500 characters.")
  }

  params <- list(
    q = keyword_encoded,
    apikey = api_key,
    language = language
  )

  # Add optional parameters only if they are not NULL
  if (!is.null(from)) {
    params$from <- from
  }
  if (!is.null(to)) {
    params$to <- to
  }
  if (!is.null(sortBy)) {
    params$sortBy <- sortBy
  }

  response <- httr::GET(url = news_url, query = params)

  if (httr::status_code(response) != 200) {
    stop("Request failed with status: ", httr::status_code(response))
  }

  data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  articles_df <- as.data.frame(data$articles)
  return(articles_df[, c("title", "content", "url", "source.name", "publishedAt")])
}

#' Get live top headlines from NewsAPI "top-headlines" Endpoint
#'
#'
#' @param category A character string representing the category you want to get headlines for. (e.g., :business",
#'   "entertainment", "general", "health", "science", "sports", "technology")
#'   If NULL, this parameter is omitted from the query.
#'
#' @return A data frame containing the top headlines.
#'
#' @examples
#' \dontrun{
#'   # Retrieve headlines from all categories
#'   headlines <- get_top_headlines()
#'
#'   # Retrieve headlines from the "technology" category
#'   tech_headlines <- get_top_headlines(category = "technology")
#' }
#'
#' @export
get_top_headlines <- function(category = NULL) {
  top_headlines_url <- "https://newsapi.org/v2/top-headlines"
  api_key <- Sys.getenv("NEWS_API_KEY")
  if (api_key == "") stop("NEWS_API_KEY is not set.")

  params <- list(country = "us", apiKey = api_key)
  # Add optional parameters only if they are not NULL
  if (!is.null(category)) {
    params$category <- category
  }
  response <- httr::GET(url = top_headlines_url, query = params)

  if (httr::status_code(response) != 200) stop("Request failed with status: ", httr::status_code(response))

  data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  articles_df <- as.data.frame(data$articles)
  return(articles_df[, c("title", "content", "url", "publishedAt")])
}
