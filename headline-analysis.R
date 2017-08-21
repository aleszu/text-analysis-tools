library(tidyverse)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tableHTML)
library(wordcloud)
data(stop_words)

top500data <- read_csv("top500.csv", col_names = TRUE)

plotlyFB <- plot_ly(y=top500data$Facebook, x=nchar(top500data$Headline), type = 'scatter', mode = 'markers',
                    hoverinfo = 'text',
                    text = ~paste('Headline: ', top500data$Headline,
                                  '<br>Publisher: ', top500data$Publisher,
                                  '<br>Date: ', top500data$Date)) %>%
  layout(title = 'Headline length vs. Facebook shares',
         xaxis = list(title = 'Headline length'),
         yaxis = list(title = 'Facebook shares'))

plotlyFB

htmlwidgets::saveWidget(as.widget(plotlyFB), "plotlyFB.html")

#plotly_POST(plotlyFB, filename = "FBtop500", sharing = "public")

plotlyTW <- plot_ly(y=top500data$Twitter, x=nchar(top500data$Headline), type = 'scatter', mode = 'markers',
                    hoverinfo = 'text',
                    text = ~paste('Headline: ', top500data$Headline,
                                  '<br>Publisher: ', top500data$Publisher,
                                  '<br>Date: ', top500data$Date)) %>%
  layout(title = 'Headline length vs. Twitter shares',
         xaxis = list(title = 'Headline length'),
         yaxis = list(title = 'Twitter shares'))

plotlyTW

htmlwidgets::saveWidget(as.widget(plotlyTW), "plotlyTW.html")

#plotly_POST(plotlyTW, filename = "TWtop500")



# Plot top Publishers by total Facebook posts

top500totalfb <- read_csv("topbytotalfb.csv", col_names = TRUE)

subsettop500 <- top500totalfb %>%
  subset(top500totalfb$totalfb > 3000000)

toppubstotFB <- plot_ly(subsettop500, x = ~Publisher, y = ~totalfb, type = 'bar', name = 'Facebook', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~totaltw, name = 'Twitter', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = "Total Facebook shares"),
         margin = list(b = 100),
         barmode = 'group')

toppubstotFB

htmlwidgets::saveWidget(as.widget(toppubstotFB), "toppubstotFB.html")

# Plot top Publishers by average Facebook posts

top500avgfb <- read_csv("topbyavgfb.csv", col_names = TRUE)

subsetavgtop500 <- top500avgfb %>%
  subset(top500avgfb$avgfb > 80000)

toppubsavgFB <- plot_ly(subsetavgtop500, x = ~Publisher, y = ~avgfb, type = 'bar', name = 'Facebook', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~avgtw, name = 'Twitter', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = "Average Facebook shares"),
         margin = list(b = 100),
         barmode = 'group')

toppubsavgFB

htmlwidgets::saveWidget(as.widget(toppubsavgFB), "toppubsavgFB.html")


# Most frequent headline phrases

headlines_df <- data_frame(text = top500data$Headline)

# Tokenize the text

tokenizedText <- headlines_df %>%
  unnest_tokens(word, text) 

# Search for bigrams and trigrams

bigramsText <- headlines_df %>%
  unnest_tokens(bigram,text,token = "ngrams", n = 2)

trigramsText <- headlines_df %>%
  unnest_tokens(trigram,text,token = "ngrams", n = 3)

# Do a wordcount 

wordCount <- tokenizedText %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

# Pull out confounding words

wordCount %>%
  filter(n > 150, word != 'india',word != 'times', word != 'bbc') %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Count bigrams and trigrams 

bigramCount <- bigramsText %>%
  count(bigram, sort = TRUE) 

trigramCount <- trigramsText %>%
  count(trigram, sort = TRUE) 

# View the tables and export HTML of bigrams and trigrams

View(wordCount)
View(bigramCount)
View(trigramCount)

tableHTML(wordCount)
write_tableHTML(tableHTML(wordCount), file = 'top500wordcount.html')
tableHTML(bigramCount)
write_tableHTML(tableHTML(bigramCount), file = 'top500bigrams.html')
tableHTML(trigramCount)
write_tableHTML(tableHTML(trigramCount), file = 'top500trigrams.html')

# Plot most frequently used bigrams and trigrams (with over 15 mentions)

bigramCount %>%
  filter(n > 50) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

trigramCount %>%
  filter(n > 12) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()







