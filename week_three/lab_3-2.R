# Titanic Survivor EDA

library(ggplot2)

#########################
# Setup and data import #
#########################

setwd('E://projects/it460/week_three/')

df <- read.csv('../data/titanic.csv')

################
# Examine data #
################

str(df)

summary(df$Fare)
summary(df$Survived)


############
# Cleaning #
############

# Drop unneccesary columns
df$Name <- NULL
df$Parch <- NULL
df$Ticket <- NULL
df$Cabin <- NULL
df$PassengerId <- NULL
df$Embarked <- NULL

# Set Types
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df$SibSp <- as.numeric(df$SibSp)


################
# Text Reports #
################

# Survival
survived_table <- table(df$Survived)                  # <--------- (1)
survived_table                                        # |
survived_prop <- prop.table(survived_table)*100       # |
round(survived_prop, digits = 1)                      # |---------

# Age 
summary(df$Age)                                       # <----------(2)

# Age to Fare


###########
# Visuals #
###########

# Suvived Distribution                               # <-----------(1)
ggplot(aes(x = Survived), data = df) +
  geom_histogram(stat = 'Count') + 
  ggtitle('Survival Split') 
           
# Age boxplot
ggplot(aes(y = Age, x = 1), data = df) +            # <-------------(2)
  geom_boxplot() +
  ggtitle('Boxplot of Passenger Age')

# Age to Fare Scatter 
ggplot(aes(x = Age, y = Fare), data = df) +         #<--------------(3)
  geom_jitter(aes(color = Survived)) + 
  geom_smooth(method = lm, color = 'black') +
  labs(title = "Titanic Passenger Age by Fare Scatterplot",
       subtitle = "Most Expensive Titanic Tickets ...",
     caption = "Source: Kaggle Titanic Disaster Dataset",
     x = "Age of Passenger", y = "Fare for Ticket") 
  # ADD TEXT

# ---------------------------------------------------------------------------
# Required commands
# - what percent of passengers survived           / text report and table and bars
# - What is average age of passengers             / histogram and text report
# - show relationship between age and fare paid   / scatter plot w/ trend line & report