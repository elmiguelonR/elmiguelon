test_that("get_sentiment_r() returns Positive for clearly positive text", {
  result <- get_sentiment_r("I love sunny days!")
  expect_true(result$score > 0)
  expect_equal(result$label, "Positive")
})

test_that("get_sentiment_r() returns Negative for clearly negative text", {
  result <- get_sentiment_r("I hate everything.")
  expect_true(result$score < 0)
  expect_equal(result$label, "Negative")
})

test_that("get_sentiment_r() returns Neutral for balanced text", {
  result <- get_sentiment_r("This is an ordinary statement.")
  # Score should be close to zero => label "Neutral"
  expect_true(abs(result$score) < 0.05)
  expect_equal(result$label, "Neutral")
})
