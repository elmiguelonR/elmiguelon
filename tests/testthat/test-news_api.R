library(testthat)
library(httr)
library(mockery)


# Helper function to create a mock response
create_mock_response <- function(status_code, content) {
  response <- list(
    status_code = status_code,
    content = charToRaw(content)
  )
  class(response) <- "response"
  return(response)
}

# Test 1.
test_that("get_news_everything handles missing API key", {
  # Mock environment variable to be empty
  withr::with_envvar(c("NEWS_API_KEY" = ""), {
    # Check that the function raises an error
    expect_error(get_news_everything(keyword = "test"), "NEWS_API_KEY is not set.")
  })
})


# Test 2.
test_that("get_news_everything handles successful API calls", {

  withr::with_envvar(c("NEWS_API_KEY" = "test_api_key"), {

    # Create a mock JSON response
    mock_json <- '{
      "status": "ok",
      "totalResults": 2,
      "articles": [
        {
          "source": {"id": "test-source-1", "name": "Test Source 1"},
          "title": "Test Title 1",
          "url": "https://example.com/1",
          "publishedAt": "2025-03-16T12:00:00Z",
          "content": "Test Content 1"
        },
        {
          "source": {"id": "test-source-2", "name": "Test Source 2"},
          "title": "Test Title 2",
          "url": "https://example.com/2",
          "publishedAt": "2025-03-15T12:00:00Z",
          "content": "Test Content 2"
        }
      ]
    }'

    # Mock the GET function
    mock_get <- mock(create_mock_response(200, mock_json))

    with_mock(
      "httr::GET" = mock_get,
      "httr::content" = function(response, type, encoding) mock_json,
      {

        result <- get_news_everything(keyword = "test")

        expect_called(mock_get, 1)

        args <- mock_args(mock_get)[[1]]
        expect_equal(args$url, "https://newsapi.org/v2/everything")
        expect_equal(args$query$q, "test")
        expect_equal(args$query$language, "en")

        # Check the result
        expect_is(result, "data.frame")
        expect_equal(nrow(result), 2)
        expect_equal(result$title[1], "Test Title 1")
        expect_equal(result$content[1], "Test Content 1")
      }
    )
  })
})

# Test 3.
test_that("get_top_headlines handles missing API key", {
  # Mock environment variable to be empty
  withr::with_envvar(c("NEWS_API_KEY" = ""), {
    # Check that the function raises an error
    expect_error(get_top_headlines(), "NEWS_API_KEY is not set.")
  })
})


# Test 4.
test_that("get_top_headlines returns correct data structure", {
  # Mock environment variable
  withr::with_envvar(c("NEWS_API_KEY" = "test_api_key"), {

    # Create a mock JSON response
    mock_json <- '{
      "status": "ok",
      "totalResults": 1,
      "articles": [
        {
          "source": {"id": "test-source", "name": "Test Source"},
          "title": "Test Headline",
          "url": "https://example.com",
          "publishedAt": "2025-03-16T12:00:00Z",
          "content": "Test Content"
        }
      ]
    }'

    # Mock the GET function
    mock_get <- mock(create_mock_response(200, mock_json))

    # Stub the GET function
    with_mock(
      "httr::GET" = mock_get,
      "httr::content" = function(response, type, encoding) mock_json,
      {
        # Call the function
        result <- get_top_headlines(category = "technology")

        # Check the result structure
        expect_is(result, "data.frame")
        expect_equal(ncol(result), 4)  # Should have title, content, url, publishedAt
        expect_equal(nrow(result), 1)
        expect_equal(result$title, "Test Headline")

        # Check that category was passed correctly
        args <- mock_args(mock_get)[[1]]
        expect_equal(args$query$category, "technology")
      }
    )
  })
})
