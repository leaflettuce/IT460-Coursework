
setwd('E:/projects/it460/week_five')
set.seed(2345)

teens <- read.csv("../data/snsdata.csv")


#############
# Data Prep #
#############

# exploration
View(teens)
str(teens)

table(teens$gender)

table(teens$gender, useNA = "ifany")


summary(teens$age)


# Clean Missing Data
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

# Dummy out gender
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")


# Impute age
mean(teens$age)
mean(teens$age, na.rm = TRUE)

aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# replace NA with average
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

summary(teens$age)


###############
# Train Model #
###############

# scale input data
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

# fit
teen_clusters <- kmeans(interests_z, 5)

# results
teen_clusters$size
teen_clusters$centers


#################
# improve model #
#################
teens$cluster <- teen_clusters$cluster

# print some out
teens[1:5, c("cluster","gender","age","friends")]

# further examine cluster trends
# age by cluster
aggregate(data = teens, age ~ cluster, mean)
# gender by cluster
aggregate(data = teens, female ~ cluster, mean)
# friend count by cluster
aggregate(data = teens, friends ~ cluster, mean)


###############
# Furt tuning #
###############

# k = 3
clusters_3 <- kmeans(interests_z, 3)
teens$cluster <- clusters_3$cluster

aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)

# k = 10
clusters_10 <- kmeans(interests_z, 10)
teens$cluster <- clusters_10$cluster

aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
