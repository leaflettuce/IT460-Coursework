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

#transform to same format as textbook
df$default <- ifelse(df$default == 2, 'yes', 'no') 



#######
# EDA #
#######


