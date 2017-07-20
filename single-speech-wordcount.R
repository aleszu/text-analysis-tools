library(dplyr)
library(tidytext)
library(ggplot2)
data(stop_words)

# Ingest Trump speech

fileText <- scan("trump/speech_0.txt",sep="\n",what="raw") 

# Place speech text in a dataframe so "tidy" can use it 

text_df <- data_frame(text = fileText)

# Tokenize text to place 1 token per row and remove common words like "the"

tokenizedText <- text_df %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words)

# Tokenize text into bigrams and trigrams

bigramsText <- text_df %>%
  unnest_tokens(bigram,text,token = "ngrams", n = 2) 

trigramsText <- text_df %>%
  unnest_tokens(trigram,text,token = "ngrams", n = 3) 

# Do a wordcount 

wordCount <- tokenizedText %>%
  count(word, sort = TRUE) 

# Count bigrams and trigrams 

bigramCount <- bigramsText %>%
  count(bigram, sort = TRUE) 

trigramCount <- trigramsText %>%
  count(trigram, sort = TRUE) 

# View the tables of bigrams and trigrams

View(wordCount)
View(bigramCount)
View(trigramCount)

# Plot most frequently used words (with over 10 mentions)

wordCount %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Plot most frequently used bigrams and trigrams (with over 5 mentions)

bigramCount %>%
  filter(n > 10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

trigramCount %>%
  filter(n > 5) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
