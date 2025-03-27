# Test 1
test_that("error when no api keys are found", {
  # Clear existing API keys
  Sys.setenv(NEWS_API_KEY = "")
  Sys.setenv(OPENAI_API_KEY = "")

  # Simply call the function and expect an error
  expect_error(
    search_and_translate_news(search_method = "top-headlines"),
    "NEWS_API_KEY is not set"
  )
})

# Test 2
test_that("error if keyword is missing for 'everything'", {
  expect_error(
    search_and_translate_news(search_method = "everything", keyword = NULL),
    "You must provide a 'keyword'"
  )
})

# Test 3
test_that("error if no articles found", {
  fake_empty <- data.frame(
    title = character(0),
    source.name = character(0),
    publishedAt = character(0),
    stringsAsFactors = FALSE
  )
  testthat::with_mocked_bindings(
    get_news_everything = function(keyword, from, to, sortBy) fake_empty,
    {
      expect_error(
        search_and_translate_news(
          search_method = "everything",
          keyword = "test",
          from = "2025-03-01",
          to = "2025-03-02",
          sortBy = "relevancy"
        ),
        "No articles found."
      )
    }
  )
})
