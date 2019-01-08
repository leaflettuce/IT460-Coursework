# IT460 - Week One Lab
# Andrew Trick
#pa
# Set up environment and run simple linear regression.

# imports
library(RWeka)
library(datasets)
library(ggplot2)
library(grid)
library(gridExtra)
library(corrplot)

# examine data
dim(mtcars)
str(mtcars)
head(mtcars)

# MODEL - linear reg
linreg_1 <- LinearRegression(mpg ~ ., data = mtcars)

linreg_2 <- step(lm(mpg ~ ., data = mtcars), trace = 1)

summary(linreg_1)
summary(linreg_2)

# EDA VISUALS

# Histo of MPG
ggplot(aes(x = mpg), data = mtcars) +
  geom_histogram(binwidth = 2.5) + 
  ggtitle('MPG Histogram') +
  xlab('MPG') + 
  ylab('Count')

# Scatters of MPG v. weight and qsec
p1 <- ggplot(aes(x = wt, y = mpg), data = mtcars) +
  geom_jitter(alpha = .15)  +
  geom_point(stat = 'summary', 
             fun.y = 'mean') +
  ggtitle('MPG by Weight') +
  ylab('MPG') + 
  xlab('Weight')

p2 <- ggplot(aes(x = qsec, y = mpg), data = mtcars) +
  geom_jitter(alpha = .15)  +
  geom_point(stat = 'summary', 
             fun.y = 'mean') +
  ggtitle('MPG by qsec') +
  ylab('MPG') + 
  xlab('qsec')

grid.arrange(p1, p2, ncol = 2)

# Boxplot of MPG to AM
ggplot(aes(x = mtcars$am == 1, y = mpg), data = mtcars) +
  geom_boxplot() +
  ggtitle('Automatic v. Manual') +
  ylab('MPG') + 
  xlab('Manuel = True')

# corr plot
cor <-cor(mtcars)
corrplot(cor, method="number")
