#############################
# Library Imports and Setup #
#############################
# Working Dir
setwd('e:/projects/it460/final')

# Data Import
df <- read.csv("../data/final/ad.csv")


###############
# Exploration #
###############
str(df)

summary(df[1])
summary(df[2])
summary(df[3])

hist(df$X125)
hist(df$X125.1)
hist(df$X1.0)

plot(df[0:2])

# visualize matrix

#library(plotrix)
#x11(width=12,height=5)
#df_num <- as.data.frame(lapply(df, as.numeric))
#color2D.matplot(df_num[4:1557],c(1,1,0),c(0,1,0),0,border=FALSE) 

library(ggplot2)
library(reshape2)
test_m <- melt(df[4:1558])
ggplot(test_m, aes(X1, variable, fill = value)) + geom_raster() +
  scale_fill_gradient(low = "white", high = "red")

ggplot(aes(x = ad.), data = df) + 
  geom_histogram(stat = "count")

############
# Cleaning #
############
# Set all vars as factors
df[] <- lapply(df, function(x) as.factor(x))


# Missing Values 
df[ df == "   ?" ] <- NA
df[ df == "     ?" ] <- NA
df[ df == "?" ] <- NA



# set numeric
df$X125 <- as.numeric(as.character(df$X125))
df$X125.1 <- as.numeric(as.character(df$X125.1))
df$X1.0 <- as.numeric(as.character(df$X1.0))


# replace NA's in terminology
df$X1[is.na(df$X1)] <- 0
df$X1 <- droplevels(df$X1)
  
# remove missing size data
df <- na.omit(df)

#################
# Normalization #
#################
# min max normalization
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# apply to numeric columns
df[1:3] <- as.data.frame(lapply(df[1:3], min_max_normalize))

# check it
head(df[1:3])
summary(df[1:8])

############################
# Clean classification var #
############################
summary(df$ad.)
df$ad. <- ifelse(df$ad. == "ad.", 1, 0)
df$ad. <- as.factor(df$ad.)

s#############
# Print out #
#############
write.csv(df, file = "../data/final/ad_cleaned.csv")

