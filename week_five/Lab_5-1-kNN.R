
setwd('E:/projects/it460/week_five')

df <- read.csv('../data/wisc_bc_data.csv', stringsAsFactors = FALSE)


# Exploration and cleaning
str(df)
df <- df[-1]
