library(caret)
library(tidyverse)
library(missMethods)

df <- read.csv("feature_engineering_output.csv", as.is = TRUE)

## 0. Remove Outliers
##____________________

df2 <- df %>%
  filter(price < 334 & price > 20)

## 1. Creating Dummy Variables
## ___________________________

# Assigning levels to room_type
df_1he <- df2 %>%
  mutate(
    room_type = recode_factor(room_type,
                              "Private room" = 1,
                              "Entire home/apt" = 2,
                              .default = 3  # Assign 3 to anything not matching the specified categories
    ))

# One-hot encoding neighborhood                              
dummy_model <- dummyVars(~ neighbourhood, data = df_1he)
df_encoded <- data.frame(predict(dummy_model, newdata = df_1he))


## 2. Setting up a linear model
## ___________________________

# Creating a combined data frame
data_combined <- cbind(df_1he, df_encoded)

# Replace missing values in reviews per month with 0
data_combined <- data_combined %>%
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 0, reviews_per_month))

# Replace missing values with column mean
data_combined <- data_combined %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Creating a squared predictor for minimum nights
data_combined <- mutate(data_combined, Min_Nights_Squared = minimum_nights^2)

# Specify the predictors
predictors <- c("neighbourhood", "room_type", "minimum_nights", 
                "number_of_reviews", "reviews_per_month", 
                "calculated_host_listings_count", "availability_365",
                "sentiment", "isBot", "Min_Nights_Squared")

# Create a formula for the linear regression model
formula <- as.formula(paste("price ~", paste(predictors, collapse = " + ")))


## 3. Cross Validation
## ___________________

# Specify the number of folds for cross-validation
k <- 10

# Create a control object for k-fold cross-validation
ctrl <- trainControl(method = "repeatedcv", number = k, repeats = 3)

# Train the linear regression model using k-fold cross-validation
lm_model <- train(formula, data = data_combined, method = "lm", trControl = ctrl)

# View the results
print(lm_model)
lm_results <- summary(lm_model)
adjusted_r_squared <- lm_results$adj.r.squared
print(adjusted_r_squared)