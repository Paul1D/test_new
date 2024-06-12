# Data Wrangling Decision Analysis

library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(purrr)
library(ggplot2)

urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"

participants_data <- read_csv(url(urlfile))

# Change the selection to batch and age
sub1 <-select(participants_data, 
       academic_parents,
       working_hours_per_day)

participants_data$labor_mean <- mutate(participants_data, 
       labor_mean = working_hours_per_day*
         mean(working_hours_per_day))

# Mutate new column named response_speed 
# populated by 'slow' if it took you 
# more than a day to answer my email and 
# 'fast' for others

df1 <- mutate(participants_data, 
       commute = 
         ifelse(km_home_to_zef > 10, 
                "commuter", "local"))


# Split the data frame by batch, 
# fit a linear model formula 
# (days to email response as dependent 
# and working hours as independent) 
# to each batch, compute the summary, 
# then extract the R^2.
participants_data %>%
  split(.$gender) %>% 
  map(~ 
        lm(number_of_publications ~ 
             number_of_siblings, 
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")




#Your turn to perform

#Up until this point the code has been provided for you to work on. Now it is time for you to apply your new found skills. 
#Please work through the wrangling tasks we just went though. Use the diamonds data and make the steps in long 
#format (i.e. assigning each step to an object) and short format with (i.e. with the magrittr pipeline):
  
#select: carat and price
#filter: only where carat is > 0.5
#rename: rename price as cost
#mutate: create a variable with 'expensive' if greater than mean of cost and 'cheap' otherwise
#group_by: split into cheap and expensive
#summarize: give some summary statistics of your choice
#The diamonds data is built in with the ggplot2 library. It is already available in your R environment. Look at the help file with 
#?diamonds to learn more about it.

df <- diamonds

df %>%
  select(carat, price)

df %>%
  filter(carat > 0.5)

df %>%
  rename(cost = price)

df <- mutate(df, affordability = 
               ifelse(price > mean(df$price), 
                      "expensive", "cheap"))
df%>%
  group_by(affordability)

df%>%
  filter(affordability == "expensive")

summary(df)

participants_barplot <- table(participants_data$gender)

barplot(participants_barplot)

ggplot(data = participants_data,
       aes(x=age,
           y = batch,
           color = gender,
           size =  number_of_siblings))+
  geom_point(aes(shape = gender))
