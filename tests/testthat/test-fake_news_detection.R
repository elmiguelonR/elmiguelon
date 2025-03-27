# Mock articles dataset for testing
test_articles <- data.frame(
  title = c("Apple Mac Studio (M4 Max, 2025) Review: Small but Mighty",
            "Apple MacBook Air (13-Inch, M4) Review: More Power For Less Money",
            "10 Apple reportedly challenges the UK’s secretive encryption crackdown"),
  content = c("Content of article 1", "Content of article 2", "Content of article 3"),
  url = c("https://www.wired.com/review/apple-mac-studio-2025/",
          "https://www.wired.com/review/macbook-air-13-inch-2025/",
          "https://www.theverge.com/news/623977/apple-uk-encryption-order-appeal"),
  source.name = c("Wired", "Wired", "The Verge"),
  publishedAt = c("2025-03-11T13:00:00Z", "2025-03-11T13:00:00Z", "2025-03-04T18:29:39Z"),
  stringsAsFactors = FALSE
)


# Mock environment variable for OpenAI API key
Sys.setenv(OPENAI_API_KEY = "mock_api_key")

# Unit test 1
test_that("fake_news_detection handles invalid user input", {
  with_mock(
    "readline" = function(prompt) "3",  # Invalid choice
    expect_error(fake_news_detection(test_articles), "Invalid choice. Please enter 1 or 2.")
  )
})

# Unit test 2
test_that("fake_news_detection correctly performs similarity analysis with R method", {
  with_mock(
    "readline" = function(prompt) if (grepl("analysis", prompt)) "2" else "2",  # Similarity Analysis (R)
    {
      result <- fake_news_detection(test_articles)
      expect_true(is.numeric(result$similarity_score))
      expect_true(all(result$similarity_score >= 0 & result$similarity_score <= 1))
    }
  )
})

# Unit test 3
test_that("fake_news_detection handles missing OpenAI API key", {
  with_mock(
    "readline" = function(prompt) "1",  # OpenAI-based Clickbait Detection
    {
      Sys.setenv(OPENAI_API_KEY = "")  # Temporarily unset API key
      expect_error(fake_news_detection(test_articles), "OPENAI_API_KEY is not set.")
      Sys.setenv(OPENAI_API_KEY = "mock_api_key")  # Restore API key
    }
  )
})

