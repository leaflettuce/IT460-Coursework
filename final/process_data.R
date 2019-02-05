#############################
# Library Imports and Setup #
#############################
# Library Imports


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

