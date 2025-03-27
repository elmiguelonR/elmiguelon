#' Set API Keys for NewsAPI and OpenAI API
#'
#' This function sets the environment variables for NEWS_API_KEY and OPENAI_API_KEY.
#'
#' @param news_api_key A character string containing the NewsAPI key.
#' @param openai_api_key A character string containing the OpenAI API key.
#'
#' @return Invisibly returns NULL.
#'
#' @examples
#' \dontrun{
#' # Set both API keys at once
#' save_api_keys(news_api_key = "your_news_api_key_here", 
#'               openai_api_key = "your_openai_api_key_here")
#'               
#' # Set only the News API key (will prompt for OpenAI key)
#' save_api_keys(news_api_key = "your_news_api_key_here")
#' 
#' # Set only the OpenAI API key (will prompt for News API key)
#' save_api_keys(openai_api_key = "your_openai_api_key_here")
#' 
#' # Call without arguments (will prompt for both keys)
#' save_api_keys()
#' }
#' @export
save_api_keys <- function(news_api_key = NULL, openai_api_key = NULL) {
  if (is.null(news_api_key)) {
    news_api_key <- readline(prompt = "Enter your News API key: ")
  }
  if (is.null(openai_api_key)) {
    openai_api_key <- readline(prompt = "Enter your OpenAI API key: ")
  }
  Sys.setenv(NEWS_API_KEY = news_api_key)
  Sys.setenv(OPENAI_API_KEY = openai_api_key)
  message("API keys have been saved.")
  invisible(NULL)
}