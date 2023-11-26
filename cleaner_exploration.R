# will delete this file before submission.
# this is a cleaner version of data_cleaning_and_exploration.R

#I do not want scientific notation for my graphs.
options(scipen = 999)

#libraries
library(tidyverse)
library(naniar)
library(janitor)
library(visdat)
library(ggthemes)

#import dataset
brooklyn_sales <- read_csv("data/brooklyn_sales_clean.csv")

#need to differentiate between residential and non-residential first to account for outliers

#residential dataset (class 1 and 2)
brooklyn_sales_res <- brooklyn_sales |> 
  filter(tax_class_at_sale == c("1", "2"))

brooklyn_sales_nonres <- brooklyn_sales |> 
  filter(tax_class_at_sale == c("3", "4"))

#1. What factors affect how many buildings are being sold in Brooklyn over this time period?







#2. What factors affect housing prices based on the data?

# FIGURE 1, WILL COMBINE USING COWPLOT??? SHOULD BE SIDE BY SIDE

# GRAPH OF SALES OVER TIME FOR RESIDENTIAL ONLY
brooklyn_sales_res |> 
  group_by(year_of_sale) |> 
  mutate(yearly_avg_price = mean(sale_price)) |> 
  ggplot(aes(x = year_of_sale, y = yearly_avg_price)) +
  theme_minimal() +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = c(2003, 2005, 2007,
                                2009, 2011, 2013,
                                2015, 2017)) + 
  scale_y_continuous(breaks = c(200000, 400000, 600000, 800000),
                     labels = c("$200,000", "$400,000", "$600,000", "$800,000")) +
  labs(y = "Price ($)",
       x = "Year",
       title = "Average Residential Building Sale Price by Year, Brooklyn",
       caption = "Source: NYC Department of Finance") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

# GRAPH OF SALES OVER TIME FOR NON-RESIDENTIAL ONLY
brooklyn_sales_nonres |> 
  group_by(year_of_sale) |> 
  mutate(yearly_avg_price = mean(sale_price)) |> 
  ggplot(aes(x = year_of_sale, y = yearly_avg_price)) +
  theme_minimal() +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = c(2003, 2005, 2007,
                                2009, 2011, 2013,
                                2015, 2017)) + 
  scale_y_continuous(breaks = c(400000, 800000, 1200000, 1600000, 2000000),
                     labels = c("$400,000", "$800,000", "$1,200,000", "$1,600,000", "2,000,000")) +
  labs(y = "Price ($)",
       x = "Year",
       title = "Average Non-Residential Building Sale Price by Year, Brooklyn",
       caption = "Source: NYC Department of Finance") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


#3. What neighborhoods have become the most popular?
