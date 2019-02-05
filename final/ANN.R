# Fit ANN to Processed Data
library(neuralnet)

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
val <- val[-test_split, ]

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
# prep
predictors <- colnames(train[, 1:1558]) 
fit_details <- as.formula(paste('ad. ~ ' ,paste(predictors,collapse='+')))

# BASIC
ANN_model_basic <- neuralnet(fit_details, data = train, hidden = 1,
                             lifesign = "full", stepmax = 50000)
# plot(ANN_model_basic)

# Hidden = 5,3
ANN_model_5 <- neuralnet(fit_details, data = train, hidden = c(5,3), lifesign = "full",
                         err.fct = "sse", act.fct = "logistic", stepmax = 25000)
# plot(ANN_model_5)


##############
# Validation #
##############

# BASIC     // r - 68
basic_results <- compute(ANN_model_basic, test[, 1:1558])
basic_pred_strength <- basic_results$net.result
cor(basic_pred_strength, test$ad.)

# hidden = 5,3     // r - 
results_5 <- compute(ANN_model_5, test[, 1:1558])
pred_strength_5 <- results_5$net.result
cor(pred_strength_5, test$ad.)

###########
# Results #
###########

