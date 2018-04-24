# The data set shakespeare in available in the workspace
shakespeare

# Pipe the shakespeare data frame to the next line
shakespeare %>% 
# Use count to find out how many titles/types there are
count(title, type)



# Load tidytext
library(tidytext)

tidy_shakespeare <- shakespeare %>%
  # Group by the titles of the plays
  group_by(title) %>%
  # Define a new column linenumber
  mutate(linenumber = row_number()) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, text) %>%
  ungroup()

# Pipe the tidy shakespeare data frame to the next line
tidy_shakespeare %>% 
  # Use count to find out how many times each word is used
  count(word, sort = TRUE)



  shakespeare_sentiment <- tidy_shakespeare %>%
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments("bing")) 

shakespeare_sentiment %>%
  # Find how many positive/negative words each play has
  count(title, sentiment)

  sentiment_counts <- tidy_shakespeare %>%
    # Implement sentiment analysis using the "bing" lexicon
    inner_join(get_sentiments("bing")) %>%
    # Count the number of words by title, type, and sentiment

sentiment_counts %>%
    # Group by the titles of the plays
    group_by(title) %>%
    # Find the total number of words in each play
    mutate(total = sum(n),
    # Calculate the number of words divided by the total
           percent = n / total) %>%
    # Filter the results for only negative sentiment
    filter(sentiment == "negative") %>%
    arrange(percent)



word_counts <- tidy_shakespeare %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count by word and sentiment
  count(word, sentiment)

top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()




  tidy_shakespeare %>%
  # Count by title and word
  count(title, word, sort = TRUE) %>%
  # Implement sentiment analysis using the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>%
  # Filter to only examine the scores for Macbeth that are negative
  filter(title == "The Tragedy of Macbeth", score < 0)




  sentiment_contributions <- tidy_shakespeare %>%
  # Count by title and word
  count(title, word, sort = TRUE) %>%
  # Implement sentiment analysis using the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>%
  # Group by title
  group_by(title) %>%
  # Calculate a contribution for each word in each title
  mutate(contribution = score * n / sum(n)) %>%
  ungroup()
    
sentiment_contributions



sentiment_contributions %>%
  # Filter for Hamlet
  filter(title == "Hamlet, Prince of Denmark") %>%
  # Arrange to see the most negative words
  arrange(contribution)

sentiment_contributions %>%
  # Filter for The Merchant of Venice
  filter(title == "The Merchant of Venice") %>%
  # Arrange to see the most positive words
  arrange(desc(contribution))



  tidy_shakespeare %>%
  # Implement sentiment analysis using "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count using four arguments
  count(title, type, index = linenumber %/% 70, sentiment)



  # Load the tidyr package
library(tidyr)

tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  # Spread sentiment and n across multiple columns
  spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment
  mutate(sentiment = positive - negative)



  library(tidyr)
# Load the ggplot2 package
library(ggplot2)

tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  # Put index on x-axis, sentiment on y-axis, and map comedy/tragedy to fill
  ggplot(aes(index, sentiment, fill = type)) +
  # Make a bar chart with geom_col()
  geom_col() +
  # Separate panels with facet_wrap()
  facet_wrap(~ title, scales = "free_x")



  