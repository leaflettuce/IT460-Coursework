library(gmodels)
library(caret)
library(e1071)
setwd('e:/projects/it460/final')

# import cleaned data
df <- read.csv("../data/final/ad_cleaned.csv")

prop.table(table(df$ad.))

# Drop X col
df$X <- NULL

#set factor
df$ad. <- factor(df$ad.)
df[4:1558] <- lapply(df[4:1558], function(x) as.factor(x))

str(df[1:5])
str(df[1555:1559])

hist(df$X125, breaks = c(0, .025, .05, .1, .2, 1))
hist(df$X125.1, breaks = c(0,.05, .1, .2, .3, .4, 1))
hist(df$X1.0, breaks = c(0, .01, .015, .02, .03, .035, 1))

summary(df$X125)


############
# Bin Data #
############

df$X125 <- with(df, cut(X125, 
                        breaks=quantile(X125, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                        include.lowest=TRUE))

df$X125.1 <- with(df, cut(X125.1, 
                          breaks=quantile(X125.1, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                          include.lowest=TRUE))

df$X1.0 <- with(df, cut(X1.0, 
                        breaks=quantile(X1.0, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                        include.lowest=TRUE))


str(df[1:5])


###############
# Split Data #
##############

set.seed(42)

# RNG for splitting randomly
split_rng <- sample(2368, 1658)

train <- df[split_rng, ]
test <- df[-split_rng, ]

X_train <- train[1:1558]
y_train <-train$ad.
X_test <- test[1:1558]
y_test <- test$ad.

###############
# train model #
###############

nb_model <- naiveBayes(X_train, y_train, laplace = 0)
test_pred <- predict(nb_model, X_test, type = "class")

# Evaluate
confusionMatrix(as.factor(test_pred), as.factor(y_test), positive = "1")

#########
# Refit #
#########
nb_refit <- naiveBayes(X_train, y_train, laplace = 1)
refit_pred <- predict(nb_refit, X_test, type = "class")

# Evaluate
confusionMatrix(as.factor(refit_pred), as.factor(y_test), positive = "1")
