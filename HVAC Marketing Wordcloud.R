

if (!require("wordcloud")) install.packages("wordcloud", dependencies = TRUE)
if (!require("tm")) install.packages("tm", dependencies = TRUE)
if (!require("slam")) install.packages("slam", dependencies = TRUE)

library(wordcloud)
library(tm)
library(slam)
library(rvest)
library(httr)  # for handling HTTP
library(tidyverse)

# Function to safely read HTML with redirection handling
safe_read_html <- function(url) {
    tryCatch({
        # Perform an HTTP GET request to handle redirection
        response <- GET(url)
        final_url <- response$url  # Get the final URL after any redirections

        # Check if the final URL is different from the initial URL
        if (final_url != url) {
            message(paste("Redirected to", final_url))
        }

        # Read HTML from the final URL
        read_html(final_url)
    }, error = function(e) {
        message(paste("Error reading", url, ":", e$message))
        return(NULL)
    })
}

# List of specific URLs to scrape
urls <- c("https://www.aacompanies.com",
          "https://www.aacompanies.com/our-team",
          'https://calendly.com/aacompanies',
          'https://live.vcita.com/site/AACompanies',
          'http://staging-aageothermalv4.cirrusabs.com/',
          "https://www.aacompanies.com/residential")

# Scrape and collect content
all_content <- sapply(urls, function(url) {
    message("Scraping: ", url)
    page <- safe_read_html(url)
    if (!is.null(page)) {
        Sys.sleep(1)  # Delay to prevent rate limiting
        html_text(html_nodes(page, "p"))
    } else {
        NA
    }
}, USE.NAMES = FALSE)

# Convert to a text corpus
corp <- Corpus(VectorSource(all_content))

# Define standard and custom stop words
standard_stop_words <- stopwords("en")
custom_stop_words <- c("united", "states", "someone",
                       'minimum', 'drug', 'will',
                       'bennett', 'array', 'can',
                       'greg', 'bills', 'wide',
                       'wide', 'overtime', 'different',
                       'large', 'units', 'bill',
                       'highly', 'forms', 'per',
                       'frankford', 'carries', 'inc',
                       'florida', 'dual', 'mindyaacompanies.com',
                       'whatever', 'onstopshop')
all_stop_words <- c(standard_stop_words, custom_stop_words)

# Clean and preprocess the text
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, all_stop_words)

# Convert to plain text for viewing or analysis
# processed_texts <- sapply(corp, as.character)
# print(processed_texts)

# Create a term-document matrix from the corpus
tdm <- TermDocumentMatrix(corp)

# Calculate word frequencies
word_freqs <- row_sums(tdm)

# Convert word frequencies to a dataframe
word_freqs_df <- data.frame(word = names(word_freqs),
                            freq = word_freqs,
                            stringsAsFactors = FALSE)

# Order by frequency
word_freqs_df <- word_freqs_df[order(-word_freqs_df$freq), ]

# Basic word cloud in a somewhat circular layout
# wordcloud(words = word_freqs$word, freq = word_freqs$freq, min.freq = 1,
#           max.words = 100, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

# If you want to use specific shapes, consider using wordcloud2 for more options:
# if (!require("wordcloud2")) install.packages("wordcloud2")
library(wordcloud2)
library(wordcloud)
library(RColorBrewer)

# Ensure you're using the dataframe 'word_freqs_df' created from your term-document matrix
# Create a word cloud using the dataframe columns correctly
wordcloud(words = word_freqs_df$word,
          freq = word_freqs_df$freq,
          min.freq = 3,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.1,
          colors = brewer.pal(8, "Dark2"))


# Assuming 'word_freqs_df' is correctly set up as previously discussed
# It should look something like this:
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs, stringsAsFactors = FALSE)
word_freqs_df <- word_freqs_df[order(-word_freqs_df$freq), ]
word_freqs_df <- word_freqs_df %>%
    filter(freq >= '3')


# Generate a word cloud in the shape of a star
wordcloud2(data = word_freqs_df, shape = 'star', size = 0.25)
wordcloud2(data = word_freqs_df, shape = 'circle', size = 0.25)
wordcloud2(data = word_freqs_df, shape = 'cardioid', size = 0.25)
wordcloud2(data = word_freqs_df, shape = 'diamond', size = 0.25)
wordcloud2(data = word_freqs_df, shape = 'triangle-forward', size = 0.25)
wordcloud2(data = word_freqs_df, shape = 'triangle', size = 0.25)
wordcloud2(data = word_freqs_df, shape = 'pentagon', size = 0.25)
wordcloud2(data = word_freqs_df, shape = 'star', size = 0.25)


# Adjust scaling and size parameters
wordcloud2(data = word_freqs_df,
           shape = 'star',
           scale = c(4, 0.5),
           minSize = 0.02)

# Generate the word cloud with adjusted grid size for better visibility
wordcloud2(data = word_freqs_df,
           shape = 'star',
           gridSize = 20)
