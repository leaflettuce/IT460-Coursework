library(caret)
library(e1071)
library(tm)
library(SnowballC)
library(wordcloud)
library(gmodels)

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
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) #replacePunctuation if wanted

# stem words
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# remove whitespaces
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# check
as.character(sms_corpus_clean[[1]])

############
# Tokenize #
############

# document-term matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# secon method to tokenize and clean together!
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE, 
  removePunctuation = TRUE,
  stemming = TRUE
))

sms_dtm

##########
# splits #
##########

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5574, ]

train_labels <- sms_raw[1:4169, ]$type
test_labels <- sms_raw[4170:5574, ]$type

# check
prop.table(table(train_labels))
prop.table(table(test_labels))

#############
# Visualize #
#############

# word cloud
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# split the two
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

# more clouds
wordcloud(spam$text, max.words = 50, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 50, scale = c(3, 0.5))

#####################
# Feature Reduction #
#####################

# check at least 5 occurances of term
findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

str(sms_freq_words)

# train and test
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# set to categoerical
convert_counts <- function(x) {
  x <- ifelse(x > 0, "yes", "no")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)


############
# model NB #
############    97.9% acc

nb_model <- naiveBayes(sms_train, train_labels )
nb_pred <- predict(nb_model, sms_test)

CrossTable(nb_pred, test_labels,
           prop.chisq = FALSE,
           prop.t = FALSE,
           dnn = c("predicted", "actual"))


############
# improve  #
############  97.5%

nb_lap_model <- naiveBayes(sms_train, train_labels, laplace = 1)
nb_lap_pred <- predict(nb_lap_model, sms_test)

CrossTable(nb_lap_pred, test_labels,
           prop.chisq = FALSE,
           prop.t = FALSE,
           dnn = c("predicted", "actual"))

