# Fit ANN to Processed Data
library(caret)

# import cleaned data
df <- read.csv("../data/final/ad_cleaned.csv")

# check
head(df[1:6])
head(df[1555:1560])

prop.table(table(df$ad.))

# Drop X col
df$X <- NULL

##############
# Split Data #
##############
set.seed(42)

# RNG for splitting randomly
split_rng <- sample(2368, 1658)

# For kNN splits 
train <- df[split_rng, ]
test <- df[-split_rng, ]


#############
# Fit Model #
#############
# add feature
train$keyword_count <- rowSums(train[4:1558] == 1)
test$keyword_count <- rowSums(test[4:1558] == 1)

# fit

##############
# Validation #
##############
# basic     


###########
# Results #
###########

# BASIC
confusionMatrix(as.factor(pred), as.factor(test$ad.), positive = "1")
