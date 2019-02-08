library(class)
library(gmodels)
library(caret)

set.seed(2345)
setwd('E:/projects/it460/week_five')

df <- read.csv('../data/wisc_bc_data.csv', stringsAsFactors = FALSE)


#############
# Data Prep #
#############

# Exploration and cleaning
str(df)
df <- df[-1]

View(df)
table(df$diagnosis)

# relabel diagnosis
df$diagnosis <- factor(df$diagnosis, levels = c("B", "M"), labels = c("Benign", "malignant"))
round(prop.table(table(df$diagnosis)) * 100, digits = 1)

summary(df[c("radius_mean", "area_mean", "smoothness_mean")])

#############
# Normalize #
#############

# min max normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# test function
normalize(c(1 : 5))
normalize(seq(10, 50, by=10))

# normalize df
df_n <- as.data.frame(lapply(df[2:31], normalize))

summary(df$area_mean)

##################
# Split datasets #
##################

df_train <- df_n[1:469,]
df_test <- df_n[470:569,]
df_train_labels <- df[1:469, 1]
df_test_labels <- df[470:569, 1]

###############
# Train model #
###############

test_pred <- knn(train = df_train, test = df_test, cl = df_train_labels, k = 21)


# Evaluate
CrossTable(x = df_test_labels, y = test_pred, prop.chisq = FALSE)



#################
# Improve model #
#################

# standardize data
df_z <- as.data.frame(scale(df[-1]))
summary(df_z$area_mean)

# standardized split
df_train_2 <- df_z[1:469,]
df_test_2 <- df_z[470:569,]

test_pred_z <- knn(train = df_train_2, test = df_test_2, cl = df_train_labels, k = 21)

CrossTable(df_test_labels, test_pred_z, prop.chisq = FALSE)


###############
# grid tuning #
###############
df_c <- cbind(df[, 1], df_z[,2:30])
names(df_c)[1] <- "class"


# Custom          
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

m_cust <- train(class ~ ., data = df_c, method = "knn", 
                trControl=ctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)
                
# results
m_cust

# CrossTable
m_pred <- predict(m_cust, newdata = df_test_2)
CrossTable(df_test_labels, m_pred, prop.chisq = FALSE)
