library(tidyverse)
library(plotly)

setwd('/Users/Ali2/Desktop/R/ASL/Project')
df <- read.csv("feature_engineering_output.csv", as.is = TRUE)

# Convert room type into number
df <- df %>%
  mutate(room_type = recode_factor(room_type,
                                   "Private room" = 1,
                                   "Entire home/apt" = 2,
                                   .default = 3  # Assign 3 to anything not matching the specified categories
  ))

view(df)

## Can see that less nights is generally higher prices, for both room types. But it seems to be more
## for Entire home/apt
plot_ly(
  df, x = ~minimum_nights, y = ~room_type, z = ~price,
  color = ~factor(neighbourhood_group), colors = c('#BF382A', '#0C4B8E', '#006400', 'orange', 'pink')
) %>%
  add_markers(marker = list(size = 4)) %>%
  layout(
    scene = list(xaxis = list(title = 'Minimum Nights'),
                 yaxis = list(title = 'Room Type'),
                 zaxis = list(title = 'Price'))
  )

## Zoom in more to better see that relationship. 
df_filtered_room <- df %>%
  filter(room_type %in% c(1, 2), minimum_nights < 365)

plot_ly(
  df_filtered_room, x = ~minimum_nights, y = ~room_type, z = ~price,
  color = ~factor(neighbourhood_group), colors = c('#BF382A', '#0C4B8E', '#006400', 'orange', 'pink')
) %>%
  add_markers(marker = list(size = 4)) %>%
  layout(
    scene = list(xaxis = list(title = 'Minimum Nights'),
                 yaxis = list(
                   title = '',
                   tickvals = c(1, 2),
                   ticktext = c('Private room', 'Entire home/apt')
                 ),
                 zaxis = list(title = 'Price'))
  )

## Same variables just different visualizaiton.  
plot_ly(
  df, x = ~minimum_nights, y = ~neighbourhood_group, z = ~price,
  color = ~factor(room_type), colors = c('#BF382A', '#0C4B8E', 'green')
) %>%
  add_markers(marker = list(size = 4)) %>%
  layout(
    scene = list(xaxis = list(title = 'Minimum Nights'),
                 yaxis = list(title = 'Borough'),
                 zaxis = list(title = 'Price'))
  )

## Availability and 
df_filtered <- df %>%
  filter(reviews_per_month < 13, price < 2500) 

plot_ly(
  df_filtered, x = ~reviews_per_month, y = ~availability_365, z = ~price
) %>%
  add_markers(marker = list(size = 2)) %>%
  layout(
    scene = list(xaxis = list(title = 'Review per Month'),
                 yaxis = list(title = 'Availability'),
                 zaxis = list(title = 'Price'))
  )

## Zoom 2
df_filtered2 <- df %>%
  filter(reviews_per_month < 9, price < 1200) 

plot_ly(
  df_filtered2, x = ~reviews_per_month, y = ~availability_365, z = ~price
) %>%
  add_markers(marker = list(size = 2)) %>%
  layout(
    scene = list(xaxis = list(title = 'Review per Month'),
                 yaxis = list(title = 'Availability'),
                 zaxis = list(title = 'Price'))
  )

##
plot_ly(
  df, x = ~availability_365, y = ~room_type, z = ~price,
  color = ~factor(neighbourhood_group), colors = c('#BF382A', '#0C4B8E', 'green', 'orange', 'pink')
) %>%
  add_markers(marker = list(size = 4)) %>%
  layout(
    scene = list(xaxis = list(title = 'Availibility'),
                 yaxis = list(title = 'Room Type'),
                 zaxis = list(title = 'Price'))
  )
