#installing office season 2 data
library(schrute)
head(theoffice)
season_2 <- theoffice[theoffice$season ==2,]

#installing other necessary libraries
library(dplyr)
library(tidytext)
library(ggplot2)

# creating a data frame with stop words/filler words specific to our dataset
more_stop_words <- data.frame(word = c("uh", "um", "yeah", "hey"), lexicon = c(NA, NA, NA, NA))

# First, we need to convert our lines of text into individual words. We can use unnest_tokens to decouple each sentence into their individual words. anit_join on stop_words removes filler words like (i, and, the)
office_words <- season_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(rbind(stop_words, more_stop_words))

#Finding the most frequently used words
office_words %>% 
  count(word, sort = TRUE) %>%
  arrange(desc(n))%>%
  slice(1:6) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) + 
  geom_col() + 
  labs(y=NULL)

library(tidyr)

episode_sentiment <- office_words %>%
  group_by(episode) %>%
  mutate(linenumber = row_number(), episode = episode) %>% # chronological order per episode
  inner_join(get_sentiments("bing")) %>%
  count(episode, index = linenumber %/% 50, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(episode_sentiment, aes(index, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~episode, ncol = 4)

#Based on the results: it seems that there is a distinct pattern from the sentiment analysis graph. It appears that there was more low sentiment during the beginnging and the end of the season. During the middle of the season there was not an extreme dislike or liking of the episodes.