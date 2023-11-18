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

#consolidated data cleaning function
brooklyn_sales <- read_csv("data/brooklyn_sales_map.csv", na = c("NA", "")) |> 
  clean_names() |> 
  select(where(~mean(is.na(.)) < 0.5)) |> 
  subset(
    select = -c(
      borough,
      mappluto_f,
      version,
      pluto_map_id,
      x_coord,
      y_coord,
      condo_no,
      bbl,
      boro_code,
      owner_name
    )
  ) |> 
  mutate(
    land_sqft = as.numeric(land_sqft),
    zip_code = as.factor(zip_code),
    school_dist = as.factor(school_dist)
  )

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

#based on these, there seems to be an increase in building prices that may correlate
# with the lower sales after the recession... let's look into that later too.

#ok let's explore the peaks aka 2003 to 2006 first. what's selling? why's it selling?

#what types of buildings are being sold the most? counts? tax_class?
#need to zoom in. it's mainly homes and apartments, also some interesting things with vacant land?
brooklyn_sales |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006)) |> 
  group_by(building_class_category) |> 
  mutate(n = n()) |> 
  filter(n > 500) |> 
  ggplot(aes(y = fct_reorder(building_class_category, n))) +
  geom_bar()

#we also can look at tax classes, may be cleaner to look at.
#as expected, the highest # of buildings sold are class 1. might not use this graph cause need to explain key.
brooklyn_sales |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006),
         is.na(tax_class) == FALSE) |> 
  group_by(tax_class) |> 
  mutate(n = n()) |> 
  ggplot(aes(y = fct_reorder(tax_class, n))) +
  geom_bar()

#densities of prices? useless.
brooklyn_sales |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006)) |> 
  ggplot(aes(x = sale_price)) +
  geom_density()

# also let's look at avg prices of these types of buildings. (useless)
brooklyn_sales |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006)) |> 
  group_by(building_class_category) |> 
  mutate(avg_building_class_price = mean(sale_price)) |> 
  ggplot(aes(y = avg_building_class_price, x = building_class_category)) +
  geom_point()

#what's your average home prices? let's focus on residential, so anything w/ a 1 or 2
#seem to converge around below 450k max! that's crazy to me.
brooklyn_sales |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006),
         tax_class == c("1", "1A", "1B", "1C", "2", "2A", "2B", "2C")) |> 
  group_by(tax_class) |> 
  ggplot(aes(y = tax_class, x = sale_price)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 1000000))

#ok now i want to look at 2007-2010 range cause that's peak recession
brooklyn_sales |> 
  filter(year_of_sale == c(2007, 2008, 2009, 2010),
         is.na(tax_class) == FALSE) |> 
  group_by(tax_class) |> 
  mutate(n = n()) |> 
  ggplot(aes(y = fct_reorder(tax_class, n))) +
  geom_bar()

#trends in the same way. housing still expensive though.
#might be saying more about who CAN afford to buy though at the time.
brooklyn_sales |> 
  filter(year_of_sale == c(2007, 2008, 2009, 2010),
         tax_class == c("1", "1A", "1B", "1C", "2", "2A", "2B", "2C")) |> 
  group_by(tax_class) |> 
  ggplot(aes(y = tax_class, x = sale_price)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 1000000))

#and lastly 2011-2017
brooklyn_sales |> 
  filter(year_of_sale == c(2011, 2012, 2013, 2014, 2015, 2016, 2017),
         is.na(tax_class) == FALSE) |> 
  group_by(tax_class) |> 
  mutate(n = n()) |> 
  ggplot(aes(y = fct_reorder(tax_class, n))) +
  geom_bar()
#same trends, nothing special

#BUT THE HOUSES HAVE JUMPED! for some. the most popular (2 and 1) sort of occupy
#the same range.
brooklyn_sales |> 
  filter(year_of_sale == c(2011, 2012, 2013, 2014, 2015, 2016, 2017),
         tax_class == c("1", "1A", "1B", "1C", "2", "2A", "2B", "2C")) |> 
  group_by(tax_class) |> 
  ggplot(aes(y = tax_class, x = sale_price)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 1000000))


#ALSO, one thing to note is I want to see if the homes being sold were getting smaller if prices are same range.
#square feet might not be applicable as heavily towards communal spaces, so let's just look at typical homes (1)
brooklyn_sales |> 
  filter(year_of_sale == c(2011, 2012, 2013, 2014, 2015, 2016, 2017),
         tax_class == c("1")) |> 
  group_by(tax_class) |> 
  ggplot(aes(y = tax_class, x = land_sqft)) +
  geom_violin() +
  labs(
    title = "2011-2017 distribution of Square Ft in Sold Homes, Brooklyn"
  )

brooklyn_sales |> 
  filter(year_of_sale == c(2007, 2008, 2009, 2010),
         tax_class == c("1")) |> 
  group_by(tax_class) |> 
  ggplot(aes(y = tax_class, x = land_sqft)) +
  geom_violin() +
  labs(
    title = "2007-2010 distribution of Square Ft in Sold Homes, Brooklyn"
  )

brooklyn_sales |> 
  filter(year_of_sale == c(2003, 2004, 2005, 2006),
         tax_class == c("1")) |> 
  group_by(tax_class) |> 
  select(land_sqft) |> 
  summary()

brooklyn_sales |> 
  filter(year_of_sale == c(2007, 2008, 2009, 2010),
         tax_class == c("1")) |> 
  group_by(tax_class) |> 
  select(land_sqft) |> 
  summary()
  
brooklyn_sales |> 
  filter(year_of_sale == c(2011, 2012, 2013, 2014, 2015, 2016, 2017),
         tax_class == c("1")) |> 
  group_by(tax_class) |> 
  select(land_sqft) |> 
  summary()

#not many changes, maybe combine these into a single table...
#doesn't seem to affect much

#additionally, zip code analysis! do we buy more of a certain zip code. are they move $$$$
brooklyn_sales |> 
  group_by(zip_code) |> 
  mutate(n = n()) |> 
  filter(n > 12500) |> 
  ggplot(aes(y = fct_reorder(zip_code, n))) +
  geom_bar()

#need to work with fct_reorder??? not ordering right now
#note: needed to change .fun = sum. :c
brooklyn_sales |> 
  group_by(zip_code) |> 
  mutate(n = n()) |> 
  filter(n > 12500) |> 
  ggplot(aes(x = sale_price, y = fct_reorder(zip_code, sale_price, .fun = sum))) +
  geom_col()

#last off, let's do some basic analyses involving school districts. are the most popular spots expensive?
# let's use geom_segment cause that's more interesting.
brooklyn_sales |> 
  filter(is.na(school_dist) == FALSE) |> 
  group_by(school_dist) |> 
  mutate(
    n = n(),
    avg_school_price = mean(sale_price)
  ) |> 
  ggplot(aes(x = school_dist, y = avg_school_price)) +
  geom_col()

#try some correlational exploration w/ price, do some graphs w/ geom_line
brooklyn_sales |> 
  ggplot(aes(y = gross_sqft, x = sale_price, alpha = 0.8)) + 
  geom_line() +
  geom_jitter() +
  geom_line()

#try age of the property, see if correlation w/ price...
#probably a lubridate function... remove those w/ no date.


#also may need to redo some parts to check historical districts and see if those are relatively more expensive.


#older code that I may or may or may not use.
brooklyn_sales <- subset(brooklyn_sales, select = -c(borough, Borough, easement, apartment_number))

unique(brooklyn_sales$ZipCode)

brooklyn_sales |> 
  filter(year_of_sale == 2007) |> 
  ggplot(aes(x=gross_sqft, y = sale_price, alpha = 0.8)) + 
  geom_point() + 
  geom_jitter()

