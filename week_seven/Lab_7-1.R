library(neuralnet)
library(caret)


setwd("E:/projects/it460/week_seven")

# import data
concrete <- read.csv("../data/concrete.csv")
str(concrete)


############
# Cleaning # 
############

# min-max nomralize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
# check
summary(concrete_norm$strength)
summary(concrete$strength)


##############
# Split Data #
##############

concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774: 1030, ]


#################
# model fitting #
#################

m <- 