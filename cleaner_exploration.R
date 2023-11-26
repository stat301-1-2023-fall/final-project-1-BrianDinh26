# will delete this file before submission.
# this is a cleaner version of data_cleaning_and_exploration.R

#color scheme:
#residential: #d3e3e1
#non-residential: #e9f2b8

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



#1. How many buildings are being sold in Brooklyn over time? How have these sales changed? Where are they being sold?

#actual first figure int his section will be the tax class one, why? Because being residential
#or non-residential affects your price A TON. Outliers are bad, have separate analyses.

#also outlier demonstration with tax classes. 2 different figures at the start.

#FIGURE 1 (combination of these 2 graphs of residential and noresidental sales, compared side-by-side)
brooklyn_sales_res |> 
  group_by(year_of_sale) |> 
  ggplot() +
  theme_minimal() +
  geom_bar(aes(y = year_of_sale), fill ="#d3e3e1", color = "black") +
  scale_y_reverse(breaks = c(2003, 2004, 2005, 2006,
                             2007, 2008, 2009, 2010,
                             2011, 2012, 2013, 2014,
                             2015, 2016, 2017)) +
  scale_x_continuous(labels = c("0", "5,000", "10,000", "15,000")) +
  labs(title = "Number of Residential Building Sales in Brooklyn by Year",
       subtitle = "From 2003 to 2017",
       x = "Number of Sales",
       y = "Year of\nSale",
       caption = "Source: NYC Department of Finance") +
  theme(
    axis.title.y = element_text(angle = 0, 
                                vjust = 0.5,
                                margin = margin(t = 0, r = 10, b = 0, l = 0))
  )

brooklyn_sales_nonres |> 
  group_by(year_of_sale) |> 
  ggplot() +
  theme_minimal() +
  geom_bar(aes(y = year_of_sale), fill ="#e9f2b8", color = "black") +
  scale_y_reverse(breaks = c(2003, 2004, 2005, 2006,
                             2007, 2008, 2009, 2010,
                             2011, 2012, 2013, 2014,
                             2015, 2016, 2017)) +
  labs(title = "Number of Non-Residential Building Sales in Brooklyn by Year",
       subtitle = "From 2003 to 2017",
       x = "Number of Sales",
       y = "Year of\nSale",
       caption = "Source: NYC Department of Finance") +
  theme(
    axis.title.y = element_text(angle = 0, 
                                vjust = 0.5,
                                margin = margin(t = 0, r = 10, b = 0, l = 0))
  )

#analysis says there's spikes for 2003-2006, lows for 2007-2010, resurgence 2010-2017.

# FIGURE 2. COMBINE w/ cowplot. prices lower, so more sales? but also cheaper yet less sales in recession

# GRAPH OF AVG SALE PRICE OVER TIME FOR RESIDENTIAL ONLY
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

# FIGURE 3: How many of each type of building (in terms of price bracket) is being sold in each time period?
#need to make some facet cuts.

#base plot. expand into filter LATER and make tihs nice first.
#also make colors prettier.
brooklyn_sales_res |> 
  filter(sale_price > 10) |>
  mutate(
    sale_price = cut(sale_price, breaks = c(0, 200000, 300000, 400000, 500000, 600000, 700000,
                                            800000, 900000, 1000000, 1250000, 1500000, 1750000,
                                            2000000, 2500000, 3000000, 6000000))
  ) |> 
  filter(is.na(sale_price) == FALSE) |> 
  ggplot() +
  geom_bar(aes(x = sale_price), color = "black", fill = "#d3e3e1") +
  scale_x_discrete(
    labels = c("< $200k", "$200k-$300k", "$300k-$400k", "$400k-$500k", "$500k-$600k",
               "$600k-$700k", "$700k-$800k", "$800k-$900k", "$900k-$1m", "$1m-$1.25m",
               "$1.25m-$1.5m", "$1.5m-$1.75m", "$1.75m-$2m", "$2m-$2.5m", "$2.5m-$3m",
               "$3m-$6m")
  ) +
  scale_y_continuous(
    breaks = c(0, 5000, 10000, 15000),
    labels = c("0", "5,000", "10,000", "15,000")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 4), # stick with 4 cause fits html well.
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) +
  labs(
    title = "Distribution of Count of Residential Buildings Sold in Brooklyn by Price",
    subtitle = "Overall (2003 to 2017)",
    x = "Sale Price Ranges",
    y = "Count"
  )

#year-by-year distribution

#2003-2006
brooklyn_sales_res |> 
  filter(sale_price > 10,
         year_of_sale == c(2003, 2004, 2005, 2006)) |>
  mutate(
    sale_price = cut(sale_price, breaks = c(0, 200000, 300000, 400000, 500000, 600000, 700000,
                                            800000, 900000, 1000000, 1250000, 1500000, 1750000,
                                            2000000, 2500000, 3000000, 6000000))
  ) |> 
  filter(is.na(sale_price) == FALSE) |> 
  ggplot() +
  geom_bar(aes(x = sale_price), color = "black", fill = "#d3e3e1") +
  scale_x_discrete(
    labels = c("< $200k", "$200k-$300k", "$300k-$400k", "$400k-$500k", "$500k-$600k",
               "$600k-$700k", "$700k-$800k", "$800k-$900k", "$900k-$1m", "$1m-$1.25m",
               "$1.25m-$1.5m", "$1.5m-$1.75m", "$1.75m-$2m", "$2m-$2.5m", "$2.5m-$3m",
               "$3m-$6m")
  ) +
  scale_y_continuous(
    breaks = c(500, 1000, 1500, 2000)
  ) +
  coord_cartesian(ylim = c(0, 2000)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 4), # stick with 4 cause fits html well.
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) +
  labs(
    title = "Distribution of Count of Residential Buildings Sold in Brooklyn by Price",
    subtitle = "2003 to 2006",
    x = "Sale Price Ranges",
    y = "Count"
  )

#2007-2010
brooklyn_sales_res |> 
  filter(sale_price > 10,
         year_of_sale == c(2007, 2008, 2009, 2010)) |>
  mutate(
    sale_price = cut(sale_price, breaks = c(0, 200000, 300000, 400000, 500000, 600000, 700000,
                                            800000, 900000, 1000000, 1250000, 1500000, 1750000,
                                            2000000, 2500000, 3000000, 6000000))
  ) |> 
  filter(is.na(sale_price) == FALSE) |> 
  ggplot() +
  geom_bar(aes(x = sale_price), color = "black", fill = "#d3e3e1") +
  scale_x_discrete(
    labels = c("< $200k", "$200k-$300k", "$300k-$400k", "$400k-$500k", "$500k-$600k",
               "$600k-$700k", "$700k-$800k", "$800k-$900k", "$900k-$1m", "$1m-$1.25m",
               "$1.25m-$1.5m", "$1.5m-$1.75m", "$1.75m-$2m", "$2m-$2.5m", "$2.5m-$3m",
               "$3m-$6m")
  ) +
  scale_y_continuous(
    breaks = c(500, 1000, 1500, 2000)
  ) +
  coord_cartesian(ylim = c(0, 2000)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 4), # stick with 4 cause fits html well.
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) +
  labs(
    title = "Distribution of Count of Residential Buildings Sold in Brooklyn by Price",
    subtitle = "test to test",
    x = "Sale Price Ranges",
    y = "Count"
  )

#2011-2017
brooklyn_sales_res |> 
  filter(sale_price > 10,
         year_of_sale == c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) |>
  mutate(
    sale_price = cut(sale_price, breaks = c(0, 200000, 300000, 400000, 500000, 600000, 700000,
                                            800000, 900000, 1000000, 1250000, 1500000, 1750000,
                                            2000000, 2500000, 3000000, 6000000))
  ) |> 
  filter(is.na(sale_price) == FALSE) |> 
  ggplot() +
  geom_bar(aes(x = sale_price), color = "black", fill = "#d3e3e1") +
  scale_x_discrete(
    labels = c("< $200k", "$200k-$300k", "$300k-$400k", "$400k-$500k", "$500k-$600k",
               "$600k-$700k", "$700k-$800k", "$800k-$900k", "$900k-$1m", "$1m-$1.25m",
               "$1.25m-$1.5m", "$1.5m-$1.75m", "$1.75m-$2m", "$2m-$2.5m", "$2.5m-$3m",
               "$3m-$6m")
  ) +
  scale_y_continuous(
    breaks = c(500, 1000, 1500, 2000)
  ) +
  coord_cartesian(ylim = c(0, 2000)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 4), # stick with 4 cause fits html well.
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) +
  labs(
    title = "Distribution of Count of Residential Buildings Sold in Brooklyn by Price",
    subtitle = "2011 to 2017",
    x = "Sale Price Ranges",
    y = "Count"
  )

#need to make a fixed coord cartesian for each for PROPER comparison.
# general conclusion is that people are successfully buying less, and despite
# $400k-$500k being the consistent highest, there's a general upwards trend in price


# FIGURE 4: What areas are the most popular? (include a map) as reference for zip codes.
#psuedo code is group by zip code, then mutate to get variable w/ count of zip code
#in 2003, then a similar mutate to get variable w/ count of zip code in 2017.
#mutate those 2 variables, 2017 divided by 2003 count, then percent (add label? for plot)
#deviating bars horziontal?
#ignore above that doesn't work cause there are just in general less buildings being sold
#go with facet reordered ascending horizontal barchart w/ most popular zip codes, top 5

#time-based analysis? or are the same things the most popular...

brooklyn_sales_res |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006)) |> 
  mutate(zip_code = as.factor(zip_code)) |> 
  group_by(zip_code) |> 
  mutate(n = n()) |> 
  filter(n > 650) |> 
  ggplot(aes(y = fct_reorder(zip_code, n))) +
  geom_bar() +
  labs(
    title = "2003-2006 sales"
  )

brooklyn_sales_res |> 
  filter(year_of_sale == c(2007, 2008, 2009, 2010)) |> 
  mutate(zip_code = as.factor(zip_code)) |> 
  group_by(zip_code) |> 
  mutate(n = n()) |> 
  filter(n > 374) |> 
  ggplot(aes(y = fct_reorder(zip_code, n))) +
  geom_bar() +
  labs(
    title = "2007-2010 sales"
  )


brooklyn_sales_res |> 
  filter(year_of_sale == c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) |> 
  mutate(zip_code = as.factor(zip_code)) |> 
  group_by(zip_code) |> 
  mutate(n = n()) |> 
  filter(n > 400) |> 
  ggplot(aes(y = fct_reorder(zip_code, n))) +
  geom_bar() +
  labs(
    title = "2011-2017 sales"
  )
#generally the same areas tend to be popular it seems. closer to the water, the better?

# FIGURE 5: Old age and buildings? How many of each type of building in terms of age bracket are being sold
# and how does age affect price? (2 figures really)
#scatterplot is possible here? age by building price maybe.

#kind of similar to figure 2, but get average prices for each year then graph with line and
# a geom_smooth line?



#2. What factors affect housing prices based on the data?

#changing price over time faceted by tax class type...?

# FIGURE 1: Square ft, size, and # of units analysis.

# FIGURE 2: Proximity analysis, facet wrap graph?

# FIGURE 3: ZIP Code Analysis? Check if possible if they have same size or not (sq ft) and if there's a general thing.
# Price per sq ft for each zip code?

# Figure 4: School District Analysis? Unsure if any different from ZIP code, but explore!

class(brooklyn_sales$sale_price)
