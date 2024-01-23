library(tidyverse)
df <- read.csv("feature_engineering_output.csv", as.is = TRUE)

## Checking which columns have missing values 
view(as.data.frame(colSums(is.na(df))))

