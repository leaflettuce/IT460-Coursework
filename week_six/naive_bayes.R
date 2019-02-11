library(caret)
library(e1071)
library(tm)
library(SnowballC)

setwd("E:/projects/it460/week_six")

# import data and explore
sms_raw <- read.csv("../data/sms_spam.csv")
str(sms_raw)

# set class as factor
sms_raw$type <- factor(sms_raw$type)

str(sms_raw$type)
table(sms_raw$type)


######################
# text preprocessing #
######################

sms_corpus <- VCorpus(VectorSource(sms_raw$text))

print(sms_corpus)
inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])

lapply(sms_corpus[1:2], as.character)

# set to lower case
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# compare
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

# remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

# remove stopwords
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

# replace punctuation
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x)
}

# remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, replacePunctuation)

# stem words
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# remove whitespaces
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# check
as.character(sms_corpus_clean[[1]])

############
# Tokenize #
############

