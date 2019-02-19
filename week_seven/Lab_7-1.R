library(neuralnet)
library(caret)

set.seed(42)
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
# ~70/30 split
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774: 1030, ]


#################
# model fitting #
#################
 # fit neuralnet ann
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + 
                 superplastic + coarseagg + fineagg + age, 
               data = concrete_train)

# visualize
plot(concrete_model)

# evaluate
model_results <- compute(concrete_model, concrete_test[1:8])
pred_str <- model_results$net.result

# correlation
cor(pred_str, concrete_test$strength)


#################
# Improve Model #
#################
# 5 hidden neurals in one layer
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water +
                               superplastic + coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

# vis
plot(concrete_model2)

# evaluate
model_results2 <- compute(concrete_model2, concrete_test[1:8])
pred_str2 <- model_results2$net.result

# correlation
cor(pred_str2, concrete_test$strength)
