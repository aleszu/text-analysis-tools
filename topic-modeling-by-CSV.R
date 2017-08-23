library(tidytext)
library(dplyr)
library(stringr)

# Topic models with CSV on "headlines"
# Based on https://github.com/pablobarbera/eui-text-workshop/blob/master/03-unsupervised/01-topic-models.Rmd

library(topicmodels)
top500topics <- read.csv("top500_datetime.csv", stringsAsFactors = FALSE)
library(quanteda)
top500corpus <- corpus(top500topics$Headline)
top500dfm <- dfm(top500corpus, ngrams=c(1,2), ignoredFeatures=stopwords("english"))
top500dfm <- trim(top500dfm, minDoc = 2)

# we now export to a format that we can run the topic model with
dtm <- convert(top500dfm, to="topicmodels")

# estimate LDA with 50 topics
K <- 50
lda <- LDA(dtm, k = K, method = "Gibbs", 
           control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

topics <- get_topics(lda, 1)
head(topics)

# Add predicted topic column to dataset

top500topics$pred_topic <- topics

# Topics generated (these are manually organized by looking at top terms for each topic)

# Planned Parenthood, 1
# Mike Pence, 2
# White House, 13
# Supreme Court, 17
# Clinton, 19
# Women's March, 26
# Health Care, 28
# Betsy DeVos, 31
# Dakota Access, 34
# Obama, 36
# Meryl Streep, 38
# Police, 39
# Immigration, 44
# Trump, 48

# Inspect terms within topics
terms <- get_terms(lda, 15)
terms[,1]
terms <- get_terms(lda, 15)
terms[,2]
terms <- get_terms(lda, 15)
terms[,3]
terms <- get_terms(lda, 15)
terms[,4]
terms <- get_terms(lda, 15)
terms[,5]
terms <- get_terms(lda, 15)
terms[,6]
terms <- get_terms(lda, 15)
terms[,7]
terms <- get_terms(lda, 15)
terms[,8]
terms <- get_terms(lda, 15)
terms[,9]
terms <- get_terms(lda, 15)
terms[,10]
terms <- get_terms(lda, 15)
terms[,11]
terms <- get_terms(lda, 15)
terms[,12]
terms <- get_terms(lda, 15)
terms[,13]
terms <- get_terms(lda, 15)
terms[,14]
terms <- get_terms(lda, 15)
terms[,15]
terms <- get_terms(lda, 15)
terms[,16]
terms <- get_terms(lda, 15)
terms[,17]
terms <- get_terms(lda, 15)
terms[,18]
terms <- get_terms(lda, 15)
terms[,19]
terms <- get_terms(lda, 15)
terms[,20]
terms <- get_terms(lda, 15)
terms[,21]
terms <- get_terms(lda, 15)
terms[,22]
terms <- get_terms(lda, 15)
terms[,23]
terms <- get_terms(lda, 15)
terms[,24]
terms <- get_terms(lda, 15)
terms[,25]
terms <- get_terms(lda, 15)
terms[,26]
terms <- get_terms(lda, 15)
terms[,27]
terms <- get_terms(lda, 15)
terms[,28]
terms <- get_terms(lda, 15)
terms[,29]
terms <- get_terms(lda, 15)
terms[,30]
terms <- get_terms(lda, 15)
terms[,31]
terms <- get_terms(lda, 15)
terms[,32]
terms <- get_terms(lda, 15)
terms[,33]
terms <- get_terms(lda, 15)
terms[,34]
terms <- get_terms(lda, 15)
terms[,35]
terms <- get_terms(lda, 15)
terms[,36]
terms <- get_terms(lda, 15)
terms[,37]
terms <- get_terms(lda, 15)
terms[,38]
terms <- get_terms(lda, 15)
terms[,39]
terms <- get_terms(lda, 15)
terms[,40]
terms <- get_terms(lda, 15)
terms[,41]
terms <- get_terms(lda, 15)
terms[,42]
terms <- get_terms(lda, 15)
terms[,43]
terms <- get_terms(lda, 15)
terms[,44]
terms <- get_terms(lda, 15)
terms[,45]
terms <- get_terms(lda, 15)
terms[,46]
terms <- get_terms(lda, 15)
terms[,47]
terms <- get_terms(lda, 15)
terms[,48]
terms <- get_terms(lda, 15)
terms[,49]
terms <- get_terms(lda, 15)
terms[,50]

# Inspect topic terms and random article
paste(terms[,1], collapse=", ")
sample(top500topics$Headline[topics==1], 1)
paste(terms[,2], collapse=", ")
sample(top500topics$Headline[topics==2], 1)
paste(terms[,3], collapse=", ")
sample(top500topics$Headline[topics==3], 1)
paste(terms[,4], collapse=", ")
sample(top500topics$Headline[topics==4], 1)
paste(terms[,5], collapse=", ")
sample(top500topics$Headline[topics==5], 1)
paste(terms[,6], collapse=", ")
sample(top500topics$Headline[topics==6], 1)
paste(terms[,7], collapse=", ")
sample(top500topics$Headline[topics==7], 1)
paste(terms[,8], collapse=", ")
sample(top500topics$Headline[topics==8], 1)
paste(terms[,9], collapse=", ")
sample(top500topics$Headline[topics==9], 1)
paste(terms[,10], collapse=", ")
sample(top500topics$Headline[topics==10], 1)
paste(terms[,11], collapse=", ")
sample(top500topics$Headline[topics==11], 1)
paste(terms[,12], collapse=", ")
sample(top500topics$Headline[topics==12], 1)
paste(terms[,13], collapse=", ")
sample(top500topics$Headline[topics==13], 1)
paste(terms[,14], collapse=", ")
sample(top500topics$Headline[topics==14], 1)
paste(terms[,15], collapse=", ")
sample(top500topics$Headline[topics==15], 1)
paste(terms[,16], collapse=", ")
sample(top500topics$Headline[topics==16], 1)
paste(terms[,14], collapse=", ")
sample(top500topics$Headline[topics==14], 1)
paste(terms[,15], collapse=", ")
sample(top500topics$Headline[topics==15], 1)
paste(terms[,16], collapse=", ")
sample(top500topics$Headline[topics==16], 1)
paste(terms[,17], collapse=", ")
sample(top500topics$Headline[topics==17], 1)
paste(terms[,18], collapse=", ")
sample(top500topics$Headline[topics==18], 1)
paste(terms[,19], collapse=", ")
sample(top500topics$Headline[topics==19], 1)
paste(terms[,20], collapse=", ")
sample(top500topics$Headline[topics==20], 1)
paste(terms[,21], collapse=", ")
sample(top500topics$Headline[topics==21], 1)
paste(terms[,22], collapse=", ")
sample(top500topics$Headline[topics==22], 1)
paste(terms[,23], collapse=", ")
sample(top500topics$Headline[topics==23], 1)
paste(terms[,24], collapse=", ")
sample(top500topics$Headline[topics==24], 1)
paste(terms[,25], collapse=", ")
sample(top500topics$Headline[topics==25], 1)
paste(terms[,26], collapse=", ")
sample(top500topics$Headline[topics==26], 1)
paste(terms[,27], collapse=", ")
sample(top500topics$Headline[topics==27], 1)
paste(terms[,28], collapse=", ")
sample(top500topics$Headline[topics==28], 1)
paste(terms[,29], collapse=", ")
sample(top500topics$Headline[topics==29], 1)
paste(terms[,30], collapse=", ")
sample(top500topics$Headline[topics==30], 1)
paste(terms[,31], collapse=", ")
sample(top500topics$Headline[topics==31], 1)
paste(terms[,32], collapse=", ")
sample(top500topics$Headline[topics==32], 1)
paste(terms[,33], collapse=", ")
sample(top500topics$Headline[topics==33], 1)
paste(terms[,34], collapse=", ")
sample(top500topics$Headline[topics==34], 1)
paste(terms[,35], collapse=", ")
sample(top500topics$Headline[topics==35], 1)
paste(terms[,36], collapse=", ")
sample(top500topics$Headline[topics==36], 1)
paste(terms[,37], collapse=", ")
sample(top500topics$Headline[topics==37], 1)
paste(terms[,38], collapse=", ")
sample(top500topics$Headline[topics==38], 1)
paste(terms[,39], collapse=", ")
sample(top500topics$Headline[topics==39], 1)
paste(terms[,40], collapse=", ")
sample(top500topics$Headline[topics==40], 1)
paste(terms[,41], collapse=", ")
sample(top500topics$Headline[topics==41], 1)
paste(terms[,42], collapse=", ")
sample(top500topics$Headline[topics==42], 1)
paste(terms[,43], collapse=", ")
sample(top500topics$Headline[topics==43], 1)
paste(terms[,44], collapse=", ")
sample(top500topics$Headline[topics==44], 1)
paste(terms[,45], collapse=", ")
sample(top500topics$Headline[topics==45], 1)
paste(terms[,46], collapse=", ")
sample(top500topics$Headline[topics==46], 1)
paste(terms[,47], collapse=", ")
sample(top500topics$Headline[topics==47], 1)
paste(terms[,48], collapse=", ")
sample(top500topics$Headline[topics==48], 1)
paste(terms[,49], collapse=", ")
sample(top500topics$Headline[topics==49], 1)
paste(terms[,50], collapse=", ")
sample(top500topics$Headline[topics==50], 1)

# Write CSV with new column of predicted topics 

write.csv(top500topics, file = "top500with50predictedtopicsnew.csv")

