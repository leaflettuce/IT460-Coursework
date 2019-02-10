library(class)
library(gmodels)
library(caret)

setwd('e:/projects/it460/final')

# import cleaned data
df <- read.csv("../data/final/ad_cleaned.csv")

prop.table(table(df$ad.))

# Drop X col
df$X <- NULL

#set factor
df$ad. <- factor(df$ad.)

##############
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

# check props
prop.table(table(train$ad.))
prop.table(table(test$ad.))


###############
# train model #
###############

test_pred <- knn(train = X_train, test = X_test, cl = y_train, k = 21)


# Evaluate
confusionMatrix(as.factor(test_pred), as.factor(y_test), positive = "1")


#########
# refit #
#########

# Custom          
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

m_cust <- train(ad. ~ ., data = train, method = "knn", 
                trControl=ctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)

# results
m_cust

# CrossTable
m_pred <- predict(m_cust, newdata = test)

# Evaluate
confusionMatrix(as.factor(m_pred), as.factor(y_test), positive = "1")
