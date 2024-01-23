library(tidyverse)
library(olsrr)

dfvs <- read.csv("Airbnb_data 2.csv", as.is = TRUE)

# Convert boroughs and room types into number
dfvs <- dfvs %>%
  mutate(
    borough = recode_factor(neighbourhood_group,
                                        "Brooklyn" = 1,
                                        "Manhattan" = 2,
                                        "Queens" = 3,
                                        "Bronx" = 4,
                                        "Staten Island" = 5),
    room_type = recode_factor(room_type,
                              "Private room" = 1,
                              "Entire home/apt" = 2,
                              .default = 3  # Assign 3 to anything not matching the specified categories
    )
  )

# make model
model_simple <- lm(price ~ 
                    borough + 
                    room_type + 
                    minimum_nights + 
                    number_of_reviews + 
                    reviews_per_month + 
                    calculated_host_listings_count + 
                    availability_365, 
            data = dfvs)

## Variable Selection: simple
#____________________________

all_possible <- ols_step_all_possible(model_simple)
  all_possible <- all_possible[order(-all_possible$adjr), ]
  view(all_possible)

# plot shows that adjr levels at p=4 and Cp levels at p=2. Just to be safe, I will look at p=4.
best_subset <- ols_step_best_subset(model_simple) 
plot(best_subset) # viewing best combination of four models

best_subset %>% # Viewing this as table.
   # The table is hard to read with so many qualities so I'm removing some for simplicity
subset(select = -c(rsquare, cp, msep, adjr, predrsq, aic, sbic,sbc, fpe, apc, hsp, cp, msep)) %>%
  view()

## Variable Selection: advanced
#_______________________________

model_advance <- lm(price ~ borough + 
              room_type + 
              minimum_nights + 
              number_of_reviews + 
              reviews_per_month + 
              calculated_host_listings_count + 
              availability_365 +
              I(minimum_nights^2) +
              I(number_of_reviews^2) +
              I(reviews_per_month^2) +
              I(calculated_host_listings_count^2) +
              I(availability_365^2),
            data = dfvs)

all_possible <- ols_step_all_possible(model_advance) %>% 
  view() #view every single combination of these features and several criteria.

# plot shows that adjr levels at p=4 and Cp levels at p=2. Just to be safe, I will look at p=4.
best_subset_advance <- ols_step_best_subset(model_advance) 
plot(best_subset_advance) # viewing best combination of four models

best_subset %>% # Viewing this as table.
  # The table is hard to read with so many qualities so I'm removing some for simplicity
  subset(select = -c(rsquare, cp, msep, adjr, predrsq, aic, sbic,sbc, fpe, apc, hsp, cp, msep)) %>%
  view()

# Backwards stepwise regression
ols_step_backward_p(model_advance, prem = 0.05)