# Multiple Regression - Andrew Trick

# Setup and library import
setwd("E:/projects/it460/week_two")
library("psych")

# Data import
df <- read.csv("../data/insurance.csv", stringsAsFactors = TRUE)


#############
#### EDA ####
#############

str(df)

#charges
summary(df$charges) 
hist(df$charges)

summary(df$bmi)  # REQU ***

table(df$region)
table(df$sex)
table(df$smoker)

# correlations
cor(df[c("age", "bmi", "children", "charges")])
pairs.panels(df[c("age", "bmi", "children", "charges")]) #REQU ***

pairs.panels(df[c('charges', 'age', 'children', 'bmi', 'smoker', 'region', 'obese')])

######################
# Feature Generation #
######################

#nonlinear
df$bmi2 <- df$bmi^2  # REQU ***
df$age2 <- df$age^2

#obese threshold
df$obese <- ifelse(df$bmi >=30, 1, 0)


#########
# Model #
#########

#multiple regressions (linear and non-linear)
ins_1 <- lm(charges ~ ., data = df)
ins_2 <- lm(charges ~ age + age2 + children + bmi + smoker + region +
              obese + obese*smoker + age*smoker + age*obese + age*obese*smoker, data = df) 
ins_3 <- lm(charges ~ age + children + bmi + sex + smoker + region, data = df) # REQU ***

###########
# Results #
###########

summary(ins_1)   # adj r-squ = .756
summary(ins_2)   # adj r-squ = .865
summary(ins_3)   # adj R-squ = .749       # REQU ***

ins_res = resid(ins_2) 
plot(df$charges, ins_res, 
           ylab="Residuals", xlab="Charges", 
           main="Residual Plot") 
abline(0, 0)      