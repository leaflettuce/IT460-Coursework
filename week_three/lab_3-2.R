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
mean(df$Age, na.rm = TRUE)                            # |----------

# Age to Fare
model <- lm(df$Fare ~ df$Age)
summary(model)

df_no_na <- na.omit(df)
cor(df_no_na$Age, df_no_na$Fare)                      # <-----------(3)

df_no_na$Survived <- ifelse(df_no_na$Survived == 1,'yes', 'no')


###########
# Visuals #
###########

# Suvived Distribution                               # <-----------(1)
ggplot(aes(x = Survived, fill = 'Blue'), data = df) +
  geom_histogram(stat = 'Count') + 
  ggtitle('Survival Distribution') + 
  guides(fill=FALSE)
           
# Age boxplot
ggplot(aes(y = Age, x = 1), data = df) +            # <-------------(2)
  geom_boxplot() +
  ggtitle('Boxplot of Passenger Age')

# Age to Fare Scatter 
ggplot(aes(x = Age, y = Fare, color = "black"), data = subset(df_no_na, Fare < 200)) +         #<--------------(3)
  geom_jitter(size = 2) + 
  geom_smooth(method = lm, color = 'black', size = 1.3) +
  labs(title = "Titanic EDA: Passenger Age by Fare Paid",
       subtitle = "There is a Very Slight Positive Correlation Between Passenger Age and Ticket Cost",
       
       caption = paste("(R2 = ",signif(round(summary(model)$r.squared, 5), 2),
                        ", Intercept =",signif(round(model$coef[[1]],5 ), 3),
                        ", Slope =",signif(round(model$coef[[2]], 5),2),
                        ", P =",signif(round(summary(model)$coef[2,4], 5), 3), ")"),
     
       x = "Age of Passenger", y = "Cost for Ticket") +
      guides(color=FALSE)

# ---------------------------------------------------------------------------
# Required commands
# - what percent of passengers survived           / text report and table and bars
# - What is average age of passengers             / histogram and text report
# - show relationship between age and fare paid   / scatter plot w/ trend line & report