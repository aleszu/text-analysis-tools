library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
data(stop_words)

# Ingest all Trump speeches

path2file <- "trump"
fileList <- list.files(path2file,full.names = TRUE) 

readTextFiles <- function(file) { # Function to read in multiple texts and paste them into a tbl
  message(file)
  rawText = paste(scan(file, sep="\n",what="raw",strip.white = TRUE))
  output = tibble(filename=gsub("trump","",file),text=rawText) %>% 
    group_by(filename) %>% 
    summarise(text = paste(rawText, collapse = " "))
  return(output)
}

combinedTexts <- tibble(filename=fileList) %>% # Run the function to create a tbl of combined files
  group_by(filename) %>% 
  do(readTextFiles(.$filename))

# Tokenize text to place 1 token per row and remove common words like "the" plus other specific words

tokenizedText <- combinedTexts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "applause",
         word != "inaudible",
         word != "cheers")

# Filter by words with more than 5 mentions and create wordcloud 

trumpwords <- tokenizedText %>%
  count(word, sort = TRUE) 

filterWords <- trumpwords %>% 
  filter(n > 5) %>%
  with(wordcloud(word, n, max.words = 100))

# View most common words, organized by speech filename

words_by_speech <- tokenizedText %>%
  count(filename, word, sort = TRUE) %>%
  ungroup()

# View(words_by_speech)

# Do text freq. inverse document freq. analysis

tf_idf_trump <- words_by_speech %>%
  bind_tf_idf(word, filename, n) %>%
  arrange(desc(tf_idf))

# Plot top 25 tf_idf words

tf_idf_trump %>% 
  arrange(desc(tf_idf)) %>%
  top_n(25) %>%
  ggplot(aes(x = reorder(word, -tf_idf), y = tf_idf, fill = filename)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

# Looking for correlated and outlier speeches

library(widyr)

speech_cors <- words_by_speech %>%
  pairwise_cor(filename, word, n, sort = TRUE)

# Visualize correlated and outlier speeches

library(ggraph)
library(igraph)
set.seed(2017)

speech_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 3, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

