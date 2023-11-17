#dataset
#https://www.kaggle.com/datasets/tianhwu/brooklynhomes2003to2017/data?select=brooklyn_sales_map.csv

#load packages
options(scipen = 999)
library(tidyverse)
library(naniar)
library(janitor)
library(visdat)
library(ggthemes)

#load dataset, also changing a lot of character NA values to actual NA values.
#also clean column names
brooklyn_sales <- read_csv("data/brooklyn_sales_map.csv", na = c("NA", "")) |> 
  clean_names()

#checking some missingness
cake <- miss_var_summary(brooklyn_sales) |> t()

#removing columns whose NA values are more than 50% of the rows.
brooklyn_sales <- brooklyn_sales |> 
  select(where(~mean(is.na(.)) < 0.5))

#removing columns I deem irrelevant. Mainly things that are "irrelevant."
# For example, we know the borough is Brooklyn already, so don't need to
# repeat this. Also map data is irrelevant because I'm not graphing for this
# exploratory data analysis, although it would be interesting for the future to
# have a map-based evolution of common changes and sales over time.

irrelevant_columns <- c("borough", 
                        "mappluto_f", 
                        "version",
                        "pluto_map_id",
                        "x_coord",
                        "y_coord",
                        "condo_no",
                        "bbl",
                        "boro_code",
                        "owner_name"
                        )

brooklyn_sales <- subset(brooklyn_sales, select = -c(borough, 
                                                    mappluto_f, 
                                                    version,
                                                    pluto_map_id,
                                                    x_coord,
                                                    y_coord,
                                                    condo_no,
                                                    bbl,
                                                    boro_code,
                                                    owner_name))

# the bulk of my analysis will be based around housing price trends and what may
# contribute to higher prices, what years had the most sales, distribution, etc.

#graph of brooklyn apartment sale count
brooklyn_sales |> 
  group_by(year_of_sale) |> 
  ggplot() +
  theme_minimal() +
  geom_bar(aes(y = year_of_sale), fill ="#d3e3e1", color = "black") +
  scale_y_reverse(breaks = c(2003, 2004, 2005, 2006,
                             2007, 2008, 2009, 2010,
                             2011, 2012, 2013, 2014,
                             2015, 2016, 2017)) +
  scale_x_continuous(breaks = c(5000, 10000, 15000, 20000, 25000, 30000, 35000)) +
  labs(title = "Number of Building Sales in Brooklyn by Year",
       subtitle = "From 2003 to 2017",
       x = "Number of Sales",
       y = "Year of Sale",
       caption = "Source: NYC Department of Finance") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
#insert part about questions involved w/ this

#also for all years let's do a scatter/line of average sale price over each year.
brooklyn_sales |> 
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
       title = "Average Building Sale Price in Brooklyn by Year",
       caption = "Source: NYC Department of Finance") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

#ok let's explore the peaks aka 2003 to 2006
#what's your average home prices
#what types of buildings are being sold the most? counts? tax_class?
brooklyn_sales |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006)) |> 
  

#then recession graphs

#also may need to redo some parts to check historical districts and see if those are relatively more expensive.


#older code that I may or may or may not use.
brooklyn_sales <- subset(brooklyn_sales, select = -c(borough, Borough, easement, apartment_number))

unique(brooklyn_sales$ZipCode)

brooklyn_sales |> 
  filter(year_of_sale == 2007) |> 
  ggplot(aes(x=gross_sqft, y = sale_price, alpha = 0.8)) + 
  geom_point() + 
  geom_jitter()

