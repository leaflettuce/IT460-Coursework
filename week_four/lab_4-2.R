###################
# Library Imports #
###################
library(caret)

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
y_train <- df[train, c('default')]

X_test <- df[-train, ]
y_train <- df[-train, c('default')]


# Ensure poroportions of class are even between sets
prop.table(table(X_train$default))
prop.table(table(X_test$default))


############
# Modeling #
############


