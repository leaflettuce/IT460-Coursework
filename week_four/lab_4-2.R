###################
# Library Imports #
###################
library(caret)
library(C50)
library(gmodels) 
library(ROCR)


#########################
# Setup and data import #
#########################

setwd('E://projects/it460/week_four/')

df <- read.csv('../data/credit.csv')

################
# Examine data #
################

str(df)

table(df$checking_balance)
table(df$savings_balance)

summary(df$months_loan_duration)
summary(df$amount)


table(df$default)

################
# process data #
################

# Transform to same format as textbook
df$default <- ifelse(df$default > 1, 'yes', 'no') 


# Data Prep
set.seed(123)
train <- sample(1000, 900)

# Set train and test data
X_train <- df[train, ]
X_train$default <- NULL

y_train <- df[train, c('default')]
y_train <- data.frame(y_train)
colnames(y_train) <- "default"

X_test <- df[-train, ]
X_test$default <- NULL

y_test <- df[-train, c('default')]
y_test <- data.frame(y_test)
colnames(y_test) <- "default"

# Ensure poroportions of class are even between sets
prop.table(table(y_train))
prop.table(table(y_test))


############
# Modeling #
############

######
# C5 #    Acc - 74%  ,  Kappa - 35%,   auc - 67%
######

#train
c5_model <- C5.0(X_train, y_train$default)

# Model Results
summary(c5_model)

# Predict
c5_pred <- predict(c5_model, X_test)

# probability
c5_prob <-data.frame(predict(c5_model, X_test, type = 'prob'))
  

# Confusion Matrix
CrossTable(y_test$default, c5_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


###############
# AdaBoost 10 #   Acc - 76% , Kappa - 42%,  auc - 78%
###############

# Train
ada10_model <- C5.0(X_train, y_train$default, trials = 10)

# Summary
summary(ada10_model)

# Predict
ada10_pred <- predict(ada10_model, X_test)

# probability
ada10_prob <-data.frame(predict(ada10_model, X_test, type = 'prob'))

# Confusion Matrix
CrossTable(y_test$default, ada10_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


######################
# Further Evaluation #
######################
######
# C5 #
######

# matrix
confusionMatrix(c5_pred, y_test$default, positive = "no")

# ROC
c5_roc_pred <- prediction(predictions = c5_prob$yes, labels = y_test)
c5_perf <- performance(c5_roc_pred, measure = 'tpr', x.measure = 'fpr')

# plot
plot(c5_perf, main = "ROC surve for C5 model of defaults",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

# AUC
c5_perf.auc <- performance(c5_roc_pred, measure = "auc")
unlist(c5_perf.auc@y.values)

#########
# Ada10 #
#########

# matrix
confusionMatrix(ada10_pred, y_test$default, positive = "no")

#ROC
ada10_roc_pred <- prediction(predictions = ada10_prob$yes, labels = y_test)
ada10_perf <- performance(ada10_roc_pred, measure = 'tpr', x.measure = 'fpr')

# plot
plot(ada10_perf, main = "ROC surve for ada10 model of defaults",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

# AUC
ada10_perf.auc <- performance(ada10_roc_pred, measure = "auc")
unlist(ada10_perf.auc@y.values)