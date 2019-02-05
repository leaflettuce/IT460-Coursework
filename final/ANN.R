# Fit ANN to Processed Data
library(neuralnet)
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


## SPLIT FOR FURTHER TESTING

# Set train 
X_train <- df[split_rng, ]
X_train$ad. <- NULL

y_train <- df[split_rng, c('ad.')]
y_train <- data.frame(y_train)
colnames(y_train) <- "ad"

# set validation
X_val <- df[-split_rng, ]
X_val$default <- NULL

y_val <- df[-split_rng, c('ad.')]
y_val <- data.frame(y_val)

# set test
test_split <- c(88:198)

X_test <- X_val[test_split, ]
X_val <- X_val[-test_split, ]

y_test <- data.frame(y_val[test_split, ])
y_val <- data.frame(y_val[-test_split, ])

colnames(y_val) <- "ad"
colnames(y_test) <- "ad"

# Ensure poroportions of class are even between sets
prop.table(table(y_train))
prop.table(table(y_val))
prop.table(table(y_test))


# Write out splits for comparison models
# write.csv(X_train, file = "../data/final/splits/X_train.csv")
# write.csv(X_val, file = "../data/final/splits/X_val.csv")
# write.csv(X_test, file = "../data/final/splits/X_test.csv")
# write.csv(y_train, file = "../data/final/splits/y_train.csv")
# write.csv(y_val, file = "../data/final/splits/y_val.csv")
# write.csv(y_test, file = "../data/final/splits/y_test.csv")


# For NN splits 
train <- df[split_rng, ]
val <- df[-split_rng, ]
test <- val[test_split, ]

# no need for val
train <- rbind(train, test)
test <- val
rm(val)

# clean up before testing
rm(X_train)
rm(X_test)
rm(X_val)
rm(y_train)
rm(y_test)
rm(y_val)


#############
# Fit Model #
#############
# add feature
train$keyword_count <- rowSums(train[4:1558] == 1)
test$keyword_count <- rowSums(test[4:1558] == 1)

# prep
predictors <- colnames(train[, c(1:1558, 1560)]) 
fit_details <- as.formula(paste('ad. ~ ' ,paste(predictors,collapse='+')))

#  BASIC
basic <- neuralnet(fit_details, data =train, hidden = 1)
# REFIT
ANN_model_basic <- neuralnet(fit_details, data = train, hidden = 1,
                             lifesign = "full", stepmax = 50000,
                             act.fct = "tanh", err.fct = 'sse', 
                             algorithm = "rprop-", threshold = 0.005)
# plot(ANN_model_basic)

# Hidden = 5,1
ANN_model_5 <- neuralnet(fit_details, data = train, hidden = c(5,1), lifesign = "full",
                         err.fct = "sse", act.fct = "logistic", stepmax = 50000)
# plot(ANN_model_5)


### SIZE ONLY
size_predictors <- colnames(train[, 1:3]) 
size_details <- as.formula(paste('ad. ~ ' ,paste(size_predictors,collapse='+')))

size_model <- neuralnet(size_details, data = train, hidden = 1, lifesign = "full",
                         err.fct = "sse", act.fct = "logistic", stepmax = 50000)
plot(size_model)


# Size and Count
size_count_predictors <- colnames(train[, c(1:3, 1560)])
size_count_details <- as.formula(paste('ad. ~ ' ,paste(size_count_predictors,collapse='+')))

size_count_model <- neuralnet(size_count_details, data = train, hidden = 1, lifesign = "full",
                        err.fct = "sse", act.fct = "logistic", stepmax = 50000)
plot(size_count_model)



##############
# Validation #
##############
# basic     // kappa - 78  

# refit     // kappa - 89
basic_results <- compute(ANN_model_basic, test[, c(1:1558, 1560)])
basic_pred_strength <- basic_results$net.result
cor(basic_pred_strength, test$ad.)

# hidden = 5,1     // kappa - 65   - overfitting
results_5 <- compute(ANN_model_5, test[, c(1:1558, 1560)])
pred_strength_5 <- results_5$net.result
cor(pred_strength_5, test$ad.)

# SIE ONLY     // kappa -63
size_results <- compute(size_model, test[, 1:3])
size_pred <- size_results$net.result
cor(size_pred, test$ad.)

# SIE COUNT     // kappa -73
size_count_results <- compute(size_count_model, test[, c(1:3, 1560)])
size_count_pred <- size_count_results$net.result
cor(size_count_pred, count_test$ad.)


###########
# Results #
###########

# BASIC
confusionMatrix(as.factor(round(basic_pred_strength)), as.factor(test$ad.), positive = "1")

# hidden 5,1
pred_matrix_values <- unlist(lapply(pred_strength_5, function(x) if(x > .5) 1 else 0))
confusionMatrix(as.factor(pred_matrix_values), as.factor(test$ad.), positive = "1")

# size 
size_matrix <- unlist(lapply(size_pred, function(x) if(x > .5) 1 else 0))
confusionMatrix(as.factor(size_matrix), as.factor(test$ad.), positive = "1")

# size count
size_count_matrix <- unlist(lapply(size_count_pred, function(x) if(x > .5) 1 else 0))
confusionMatrix(as.factor(size_count_matrix), as.factor(test$ad.), positive = "1")
