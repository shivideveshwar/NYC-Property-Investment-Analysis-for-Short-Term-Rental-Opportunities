# NLP for Property reviews  

## Data Loading

library(tidyverse)
#install.packages("readxl")
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
#install.packages("tidytext")
library(tidytext)
library(tidyr)
#install.packages("syuzhet")
library(syuzhet)
#install.packages("textdata")
library(textdata)
#install.packages("sentimentr")
library(sentimentr)
#install.packages("caret")
library(caret)
#install.packages("wordcloud2")
library(wordcloud2)
library(RColorBrewer)

reviews <- read_csv("Desktop/Career/Case Studies/Datathon - NYC properties/External Data/Airbnb/reviews (1) 2.csv")
listings <- read_csv("Desktop/Career/Case Studies/Datathon - NYC properties/External Data/Airbnb/listings 2.csv")

#Checking variables 
head(reviews,3)

head(listings,3)

summary(listings)

## Data Exploration

# Create boxplot of ratings
ggplot(listings, aes(x = neighbourhood_group_cleansed, y = review_scores_rating)) + geom_boxplot() + geom_point(position = position_jitter(width = 0.2, height = 0))

## Data Cleaning

# Clean the data
df$comments <- str_to_lower(df$comments)  # Convert reviews to lowercase
df$comments <- str_replace_all(df$comments, "[[:punct:]]", "")  # Remove punctuation from reviews

# Split reviews into individual words
review_words <- reviews %>%
  unnest_tokens(word, comments)

# Remove stop words
review_words <- review_words %>%
  anti_join(stop_words)

# Count word frequencies
review_word_counts <- review_words %>%
  count(word, sort = TRUE)

write_xlsx(review_word_counts, "/Users/shivi/Desktop/Career/Case\ Studies/Datathon\ -\ NYC\ properties\\WordCloudData.xlsx")

# Create bar chart of most frequent words
ggplot(review_word_counts[1:20,], aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  labs(x = "", y = "Frequency") +
  ggtitle("Most Frequent Words in Reviews") +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Wordcloud of most frequent words
wordcloud2(review_word_counts, size=1.6, color = rep_len(c("green","blue")))

## Sentiment Analysis

# Perform sentiment analysis using the "syuzhet" method
syuzhet_vector <- get_sentiment(reviews$comments, method = "syuzhet")
summary(syuzhet_vector)

# Perform sentiment analysis using the "bing" method
bing_vector <- get_sentiment(reviews$comments, method = "bing")
summary(bing_vector)

# Perform sentiment analysis using the "afinn" method
afinn_vector <- get_sentiment(reviews$comments, method = "afinn")
summary(afinn_vector)

sentiment_scores <- data.frame(
  listing_id = reviews$listing_id,
  syuzhet_vector,
  bing_vector,
  afinn_vector
)
afinn_score_summary <- sentiment_scores %>%
  pivot_longer(cols = afinn_vector, names_to = "method", values_to = "sentiment") %>%
  group_by(listing_id) %>%
  summarise(mean_sentiment = mean(sentiment)) %>%
  arrange(desc(mean_sentiment)) %>%
  head(10)

# Create a bar plot of mean sentiment scores by listing and sentiment score method
ggplot(afinn_score_summary, aes(x = as.factor(listing_id), y = mean_sentiment)) +
  geom_bar(stat = "identity", position = "dodge", fill = "salmon") +
  labs(x = "", y = "Mean Sentiment Score") +
  ggtitle("Sentiment Analysis of Airbnb Reviews") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme_classic()