#' Get the Full Article Text from a URL and Remove the Last Sentence
#'
#' This function retrieves the full text of an article from the given URL by extracting
#' the content of `<p>` tags. It then performs several cleanup steps.
#' Finally, it removes the last two sentence of the article, under the assumption that it is
#' generally not important.
#'
#' @param url A character string containing the URL of the article.
#'
#' @return A character string containing the cleaned article text with the last sentence removed.
#'
get_full_article <- function(url) {
  # Preprocess the URL
  fixed_url <- gsub("\\\\u003d", "=", url)
  fixed_url <- gsub("\\\\", "", fixed_url)

  # Extract the <p> tags from the HTML page
  page <- rvest::read_html(fixed_url)
  paragraphs <- rvest::html_nodes(page, "p")
  text <- rvest::html_text(paragraphs)
  full_text <- paste(text, collapse = "\n")

  # Unescape Unicode sequences
  full_text <- stringi::stri_unescape_unicode(full_text)

  # Remove special characters and extra whitespace
  full_text <- gsub("\n", " ", full_text)
  full_text <- gsub("[^[:print:]\n]", "", full_text)
  full_text <- gsub("\\s+", " ", full_text)

  # Split the text into sentences (based on punctuation followed by whitespace)
  sentences <- unlist(strsplit(full_text, "(?<=[.!?])\\s+", perl = TRUE))

  # If there are more than two sentences, remove the last two sentences.
  if (length(sentences) > 2) {
    full_text <- paste(sentences[1:(length(sentences) - 2)], collapse = " ")
  }

  return(full_text)
}
