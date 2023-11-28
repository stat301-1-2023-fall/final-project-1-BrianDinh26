# will delete this file before submission.
# this is a cleaner version of data_cleaning_and_exploration.R

#color scheme:
#residential: #d3e3e1
#non-residential: #e9f2b8

#remember to have fig at start of the label for it to work.

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

#prologue figure #1:
brooklyn_sales |> 
  filter(is.na(tax_class_at_sale) == FALSE) |> 
  group_by(tax_class_at_sale) |> 
  mutate(n = n()) |>
  ggplot(aes(y = fct_reorder(tax_class_at_sale, n), fill = tax_class_at_sale)) +
  theme_minimal() +
  geom_bar() +
  labs(
    title = "2003-2017 Tax Classes of Buildings Sold in Brooklyn",
    y = "Tax Class\nAt Sale",
    x = "Count",
    caption = "Source: NYC Department of Finance"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position = "none")

#prologue figure 2
brooklyn_sales |> 
  ggplot(aes(x = sale_date, y = sale_price, color = tax_class_at_sale)) +
  theme_minimal() +
  geom_line(key_glyph = "timeseries") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Sale Date",
       y = "Sale Price",
       title = "Distribution of Brooklyn Building Sales Over Time by Tax Class",
       caption = "Source: NYC Department of Finance",
       color = "Tax Class at Sale") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

#prologue figure 3
brooklyn_sales |> 
  mutate(tax_class_at_sale = as.factor(tax_class_at_sale)) |> 
  ggplot(aes(x = sale_date, y = land_sqft, color = tax_class_at_sale)) +
  theme_minimal() +
  geom_line() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x = "Sale Date",
       y = "Square Ft",
       caption = "Source: NYC Department of Finance",
       color = "Tax Class at Sale") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

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

#attempt a histogram for this one i guess???

brooklyn_sales |> 
  filter(sale_price > 10) |> 
  group_by(sale_price, tax_class_at_sale) |> 
  ggplot(aes(x = sale_price)) +
  geom_histogram(bins = 500000) + coord_cartesian(xlim = c(0, 3000000))

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
    breaks = c(500, 1000, 1500, 2000),
    labels = scales::comma
  ) +
  coord_cartesian(ylim = c(0, 2000)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 4),
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

#should use neighborhood instead! argh! more intelligible than zip code or school district.

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
#WAIT JUST USE NEIGHBORHOOD FOR THIS
#neighborhood version
brooklyn_sales_res |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006)) |> 
  group_by(neighborhood) |> 
  mutate(n = n()) |> 
  filter(n > 650) |> 
  ggplot(aes(y = fct_reorder(neighborhood, n))) +
  geom_bar(fill = "#d3e3e1") +
  theme_minimal() +
  labs(
    y = "Neighborhood",
    x = "Count"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

brooklyn_sales_res |> 
  filter(year_of_sale == c(2007, 2008, 2009, 2010)) |> 
  group_by(neighborhood) |> 
  mutate(n = n()) |> 
  filter(n > 350) |> 
  ggplot(aes(y = fct_reorder(neighborhood, n))) +
  geom_bar(fill = "#d3e3e1") +
  theme_minimal() +
  labs(
    y = "Neighborhood",
    x = "Count"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

brooklyn_sales_res |> 
  filter(year_of_sale == c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) |> 
  group_by(neighborhood) |> 
  mutate(n = n()) |> 
  filter(n > 385) |> 
  ggplot(aes(y = fct_reorder(neighborhood, n))) +
  geom_bar(fill = "#d3e3e1") +
  theme_minimal() +
  labs(
    y = "Neighborhood",
    x = "Count"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))



# FIGURE 5: Old age and buildings? How many of each type of building in terms of age bracket are being sold
# and how does age affect price? (2 figures really)
#scatterplot is possible here? age by building price maybe.

#kind of similar to figure 2, but get average prices for each year then graph with line and
# a geom_smooth line?

#FIRST part of figure
brooklyn_sales |> 
  mutate(tax_class_at_sale = str_replace_all(tax_class_at_sale, 
                                             paste(c("1", "2"), 
                                                   collapse = "|", "$", sep = ""), 
                                             "residential")) |> 
  filter(tax_class_at_sale == c("residential"),
         is.na(year_built) == FALSE) |> 
  mutate(
    building_age = year_of_sale - year_built
  ) |> 
  filter(sale_price < 4000000,
         building_age < 250) |> 
  mutate(building_age = cut(
    building_age,
    breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 120))) |> 
  filter(is.na(building_age) == FALSE) |> 
  group_by(building_age) |> 
  ggplot(aes(y = building_age)) +
  theme_minimal() +
  scale_y_discrete(labels = c("0 to 15", "15 to 30", "30 to 45", "45 to 60",
                              "60 to 75", "75 to 90", "90 to 105", "105 to 120")) +
  scale_x_continuous(labels = scales::comma) +
  geom_bar(fill = "#d3e3e1", color = "black") +
  labs(
    caption = "Source: NYC Department of Finance",
    title = "Count of Sold Brooklyn Residential Building Ages from 2003 to 2017",
    y = "Building\nAge\n(in years)",
    x = "Count"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

#SECOND part of figure
brooklyn_sales_res |> 
  mutate(building_age = year_of_sale - year_built) |> 
  filter(sale_price < 4000000,
         sale_price > 100,
         building_age < 120,
         building_age > 0) |> 
  group_by(building_age) |> 
  mutate(age_avg_price = mean(sale_price, na.rm = TRUE)) |> 
  ggplot(aes(x = building_age, y = age_avg_price)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = lm) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x = "Building Age",
    y = "Average Sale\nPrice"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
  


#2. What factors affect housing prices based on the data?
#need to beautify these graphs.

#changing price over time faceted by tax class type...?

# FIGURE 1: Square ft, size, and # of units analysis.
brooklyn_sales_res |>
  filter(sale_price > 100,
         gross_sqft > 15) |>
  ggplot(aes(y = sale_price, x = gross_sqft)) +
  geom_point(alpha = 0.5) +
  geom_jitter(alpha = 0.5) +
  geom_smooth() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 4000000), xlim = c(0, 10000)) +
  labs(x = "Gross Square Ft",
       y = "Sale\nPrice") +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = 0.5,
    margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    )
  )) +
  scale_y_continuous(label = scales::dollar_format()) +
  scale_x_continuous(label = scales::comma_format())

# FIGURE 2: Proximity analysis, facet wrap graph?
#need to filter out NA...
#give an explanation of proximity stuff in report.
#also may need to relabel proximity codes.
brooklyn_sales_res |>
  filter(prox_code == c("1", "2", "3")) |>
  mutate(
    prox_code = as.factor(prox_code),
    prox_code = fct_collapse(
      prox_code,
      "Detached" = "1",
      "Semi-attached" = "2",
      "Attached" = "3"
    )
  ) |>
  group_by(year_of_sale, prox_code) |>
  mutate(yearly_avg_price = mean(sale_price)) |>
  ggplot(aes(x = year_of_sale, y = yearly_avg_price, color = prox_code)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year",
       y = "Average\nSale\nPrice") +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = 0.5,
    margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    )
  )) +
  scale_y_continuous(label = scales::dollar_format()) +
  scale_color_discrete(name = "Proximity Code")

#part 2
brooklyn_sales_res |>
  filter(prox_code == c("1", "2", "3")) |>
  mutate(
    prox_code = as.factor(prox_code),
    prox_code = fct_collapse(
      prox_code,
      "Detached" = "1",
      "Semi-attached" = "2",
      "Attached" = "3"
    )
  ) |>
  group_by(prox_code) |>
  ggplot() +
  geom_bar(aes(x = prox_code, fill = prox_code)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(
      angle = 0,
      vjust = 0.5,
      margin = margin(
        t = 0,
        r = 10,
        b = 0,
        l = 0
      )
    ),
    legend.position = "none"
  ) +
  labs(y = "Count",
       x = "Proximity Code") +
  scale_y_continuous(label = scales::comma_format())
  
# FIGURE 3: 
#WHAT NEIGHBORHOODS ARE THE MOST EXPENSIVE? ARE THEY THE SAME AS THE MOST POPULAR ONES?
brooklyn_sales_res |> 
  group_by(neighborhood) |> 
  summarise(
    Average = mean(sale_price),
    count = n()
  ) |> 
  filter(Average > 0) |> 
  arrange(desc(Average)) |> 
  DT::datatable(
    colnames = c(
      'Neighborhood' = 'neighborhood',
      'Count Sold' = 'count'
    ),
    caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      'Figure Whatever: ', htmltools::em('Brooklyn Neighborhoods Ranked by Average Residential Building Cost, 2003-2017.'))
  ) |> 
  DT::formatRound(columns = 2, digits = 2) |> 
  DT::formatCurrency(columns = "Average", currency = "$")

#modified function above
brooklyn_sales_res |> 
  group_by(neighborhood) |> 
  summarise(
    Average = mean(sale_price),
    count = n()
  ) |> 
  filter(Average > 0) |> 
  ggplot(aes(x = count, y = Average)) +
  geom_point() + 
  geom_jitter()

brooklyn_sales_res |> 
  group_by(neighborhood) |> 
  summarise(
    avg_sale_n = mean(sale_price),
    sq_avg = mean(land_sqft),
    count = n()
  ) |> 
  filter(avg_sale_n > 0,
         sq_avg < 10000) |> 
  ggplot(aes(x = count, y = sq_avg)) +
  geom_point(aes(color = avg_sale_n)) + 
  geom_jitter(aes(color = avg_sale_n)) +
  scale_x_continuous(label = scales::comma_format()) +
  scale_y_continuous(label = scales::comma_format()) +
  scale_color_continuous(
    name = "Average Sale Price",
    labels=c('$200,000','$400,000', '$600,000', '$800,000', '$1,000,000')) +
  theme_minimal() +
  labs(
    x = "Count",
    y = "Average\nSquare\nFeet"
  ) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


#box plot sq ft distribution
#2003-2006
brooklyn_sales |> 
  mutate(tax_class_at_sale = str_replace_all(tax_class_at_sale, 
                                             paste(c("1", "2"), 
                                                   collapse = "|", "$", sep = ""), 
                                             "residential")) |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006),
         tax_class_at_sale == c("residential"),
         land_sqft > 100) |> 
  group_by(tax_class_at_sale) |> 
  select(land_sqft) |> 
  ggplot(aes(y = tax_class_at_sale, x = land_sqft)) +
  theme_minimal() +
  geom_boxplot() +
  labs(
    title = "2003-2006 distribution of square ft in Brooklyn Residential Building Sales",
    caption = "Source: NYC Department of Finance",
    x = "Usable Square Feet"
  ) +
  coord_cartesian(xlim = c(0, 5000)) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

#2007-2010
brooklyn_sales |> 
  mutate(tax_class_at_sale = str_replace_all(tax_class_at_sale, 
                                             paste(c("1", "2"), 
                                                   collapse = "|", "$", sep = ""), 
                                             "residential")) |> 
  filter(year_of_sale == c(2007, 2008, 2009, 2010),
         tax_class_at_sale == c("residential"),
         land_sqft > 100) |> 
  group_by(tax_class_at_sale) |> 
  select(land_sqft) |> 
  ggplot(aes(y = tax_class_at_sale, x = land_sqft)) +
  theme_minimal() +
  geom_boxplot() +
  labs(
    title = "2007-2010 distribution of square ft in Brooklyn Residential Building Sales",
    caption = "Source: NYC Department of Finance",
    x = "Usable Square Feet"
  ) +
  coord_cartesian(xlim = c(0, 5000)) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

#2011-2017
brooklyn_sales |> 
  mutate(tax_class_at_sale = str_replace_all(tax_class_at_sale, 
                                             paste(c("1", "2"), 
                                                   collapse = "|", "$", sep = ""), 
                                             "residential")) |> 
  filter(year_of_sale == c(2011, 2012, 2013, 2014, 2015, 2016, 2017),
         tax_class_at_sale == c("residential"),
         land_sqft > 100) |> 
  group_by(tax_class_at_sale) |> 
  select(land_sqft) |> 
  ggplot(aes(y = tax_class_at_sale, x = land_sqft)) +
  theme_minimal() +
  geom_boxplot(fill = "blue") +
  labs(
    title = "2011-2017 distribution of square ft in Brooklyn Residential Building Sales",
    caption = "Source: NYC Department of Finance",
    x = "Usable Square Feet"
  ) +
  coord_cartesian(xlim = c(0, 5000)) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )


