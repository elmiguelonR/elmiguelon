# elmiguelon - the News Reporter R Package

[![R](https://github.com/ajayatil/elmiguelon/actions/workflows/r.yml/badge.svg?branch=main)](https://github.com/ajayatil/elmiguelon/actions/workflows/r.yml)

<img src="man/figures/elmiguelon_packagehex.png" style="position:absolute; top:0px; right:0px; width:150px;"/>

## Introduction

In today's fast-paced world, staying informed about politics, economics, and technology is more challenging than ever. **elmiguelon** is an R package designed to help you track the latest news trends by gathering trending headlines and articles from multiple sources. With built-in sentiment analysis, keyword search capabilities, data visualization, and translation features, this package streamlines news consumption, allowing you to quickly focus on what matters most.

## Objectives

**elmiguelon** extends these capabilities by integrating advanced data science techniques. Our package aims to:
- Interact with both the News API and the OpenAI API,
- Support keyword-based and category-based news retrieval,
- Enable sentiment analysis for news articles,
- Provide data visualization and summary statistics to identify trends,
- Translate articles to the user’s preferred language,
- Detect fake news by identifying clickbait headlines and comparing article content similarity.

## Installation
You can install the development version of regexcite from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ajayatil/elmiguelon")
```

## Authentication

To work with the **News API** and the **OpenAI API**, you must have valid API keys for each service:

- [News API key](https://newsapi.org/register/)  
- [OpenAI API key](https://platform.openai.com/signup)

Typically, you set these keys as environment variables so that every time you start R, they are automatically loaded.  

### Using `save_api_keys()`

This package provides a convenient function, **`save_api_keys()`**, which allows you to set your News API and OpenAI API keys at once (or individually) in your R environment file (`.Renviron`). This file is usually located in your home directory.

Below are some usage examples:

```r
# 1) Set both API keys at once
save_api_keys(news_api_key = "your_news_api_key_here",
              openai_api_key = "your_openai_api_key_here")

# 2) Call without arguments (will prompt for both keys)
save_api_keys()

```

## Usage

### `get_news_everything()`
This function retrieves news articles from the NewsAPI "everything" endpoint using a specified keyword, date range, sort criteria, and language. It requires a valid API key stored in your NEWS_API_KEY environment variable and returns a data frame containing the title, content, URL, source name, and publication date of each article. For instance, you can query for "climate change" articles or adjust parameters like date range and sort order to fine-tune your search. Usage examples include:

```{r}
# Retrieve "climate change" articles in English
climate_news <- get_news_everything(keyword = "climate")

# Retrieve popular articles in Spanish
spanish_news <- get_news_everything(keyword = "elecciones", language = "es", sortBy = "popularity")
```


### `get_top_headlines()`

This function retrieves live top headlines from the NewsAPI "top-headlines" endpoint using your NEWS_API_KEY environment variable. It allows you to fetch headlines from all categories by default or filter by a specific category (e.g., "technology", "business") when provided. The function returns a data frame containing the title, content, URL, and publication date of each article. For example:

```{r}
# Retrieve headlines from all categories
headlines <- get_top_headlines()

# Retrieve headlines from the "technology" category
tech_headlines <- get_top_headlines(category = "technology")
```

### `search_and_translate()`

This function allows you to search for news articles using either the "everything" or "top-headlines" endpoints, display a summarized list of up to 20 articles, and then interactively translate the selected article's title and content using OpenAI's API. It prompts you to choose an article by its number and to specify the target language for translation. The function seamlessly combines article retrieval, user selection, and translation into one easy-to-use workflow.

**How It Works:**

When you retrieve this function, you will encouter the below prompts:


Prompt 1 : `Enter the article number to translate: ` - choose one of the 20 displayed articles.


Prompt 2 : `Enter the preferred language: ` - type the language code or name you want the article translated into.


```{r}
# Retrieve articles about "Apple" from March 1 to March 8, 2025, then select one to translate to Korean:
search_and_translate_news(
  search_method = "everything",
  keyword = "Apple",
  from = "2025-03-01",
  to = "2025-03-08",
  sortBy = "relevancy"
)

# Retrieve top US headlines (business category), then select one to translate to English:
search_and_translate_news(
  search_method = "top-headlines",
  category = "business"
)

```

### `fake_news_detection()`

This function enables users to perform Clickbait Detection and Content Similarity Analysis on news articles retrieved using the `get_news_everything()` method.
Users can choose between leveraging OpenAI's LLM (GPT-4o Mini) or native R NLP(Natual Language Processing) packages for the analysis.

**How It Works:**

Prompt 1 : `Choose the type of fake news detection analysis` where users choose between:

`1: Clickbait Detection`
`2: Similarity Analysis`

Prompt 2 : `Choose a Text Analysis Method` where users choose between:

`1: OpenAI Advanced Language Model`
`2: R NLP-based Analysis`

When OpenAI's Advanced Language Model is selected, the function relies on the GPT-4o Mini model for analysis. 
Since NewsAPI provides only restricted article content, the function performs web scraping to retrieve the full article. Before analysis begins, users will see the message:
`Note: The function retrieves full article content via web scraping. Please wait for the analysis to complete!`. 
For Similarity Analysis, which requires multiple API calls when using OpenAI's API, the analysis is limited to the top 25 articles to optimize performance.

```{r}
# Get news articles
news <- get_news_everything(
  keyword = "Apple",
  from = "2025-03-01",
  to = "2025-03-11",
  sortBy = "popularity"
)

# Run method
result <- fake_news_detection(news)
print(result)
```

### `get_sentiment()`

This function selects the sentiment analysis method to use for a given text. For the OpenAI method, it always returns the detailed output (including the "details" field). You can choose between the OpenAI API and native R analysis (using the "bing" lexicon).

Below are some usage examples:

```r
# Using OpenAI detailed analysis
result_openai <- get_sentiment("The company's performance was outstanding!", method = "openai")
print(result_openai)
# Expected output (example):
# $score
# [1] 0.85
# $label
# [1] "Positive"
# $details
# [1] "The text conveys enthusiasm and confidence about the company's performance."

# Using native R method
result_r <- get_sentiment("The company's performance was outstanding!", method = "r")
print(result_r)
# Expected output (example):
# $score
# [1] 0.5
# $label
# [1] "Positive"
```

---

### `analyze_sentiment()`

This function applies sentiment analysis to each row of a data frame containing text data (e.g., news articles). It adds two new columns: `sentiment_score` and `sentiment_label`.

Below is an example using a dummy dataset:

```r
# Create a dummy data frame of articles
dummy_articles <- data.frame(
  title = c("Good News", "Bad News", "Neutral News"),
  content = c(
    "The company reported record profits this quarter, leading to a surge in stock prices.",
    "The recent scandal has severely damaged the company's reputation and led to a significant drop in stock prices.",
    "The meeting was held to discuss various aspects of the company's operations."
  ),
  publishedAt = c("2025-03-01T12:00:00Z", "2025-03-02T15:30:00Z", "2025-03-03T09:45:00Z"),
  stringsAsFactors = FALSE
)

# Analyze sentiment using the OpenAI method (detailed output is forced)
dummy_articles <- analyze_sentiment(dummy_articles, text_column = "content", method = "openai")
print(dummy_articles)
# Expected output: Data frame with additional columns: sentiment_score and sentiment_label.
```

---

### `plot_sentiment_distribution()`

This function generates a bar plot that visualizes the distribution of sentiment labels across the articles.

Below is an example:

```r
# Assuming 'articles' is a data frame with sentiment analysis results
plot_sentiment_distribution(articles)
# The function will display a bar plot showing counts for "Positive", "Negative", and "Neutral".
```

---

### `plot_sentiment_over_time()`

This function groups articles by publication date and plots the evolution of the average sentiment score over time using a line chart.

Below is an example:

```r
# Assuming 'articles' is a data frame with a 'publishedAt' column and sentiment_score
plot_sentiment_over_time(articles, date_column = "publishedAt", score_column = "sentiment_score")
# The function will display a line plot of average sentiment per day.
```

---

### `plot_word_cloud()`

This function generates a word cloud from the text content of the articles. It tokenizes the text, removes common stopwords, and visualizes the most frequent words.

Below is an example:

```r
# Assuming 'articles' is a data frame with a 'content' column
plot_word_cloud(articles, text_column = "content", max_words = 100)
# The function will display a word cloud of the most common words.
```

---

### `interactive_plot()`

This function allows the user to interactively select which plot to display by prompting a choice in the console. Options include the sentiment distribution (bar plot), sentiment evolution (line plot), or word cloud.

Below is an example:

```r
# This will prompt the user to choose a plot to display from the console.
interactive_plot(articles)
# Follow the on-screen instructions (enter 1, 2, 3, or 4) to see the desired plot.
```

## References

- [News API Documentation](https://newsapi.org/)
- [OpenAI Structured Outputs Guide](https://platform.openai.com/docs/overview)
- [newsanchor R Wrapper](https://github.com/CorrelAid/newsanchor)

