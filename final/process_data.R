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


# Remove NULLS
# df <- na.omit(df)

# set numeric
df$X125 <- as.numeric(as.character(df$X125))
df$X125.1 <- as.numeric(as.character(df$X125.1))
df$X1.0 <- as.numeric(as.character(df$X1.0))


# replace NA's
df$X1[is.na(df$X1)] <- 0

# remove missing size data
df <- na.omit(df)

###################
# Standardization #
###################


#############
# Print out #
#############

