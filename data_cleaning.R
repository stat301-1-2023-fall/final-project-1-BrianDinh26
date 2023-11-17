#dataset
#https://www.kaggle.com/datasets/tianhwu/brooklynhomes2003to2017/data?select=brooklyn_sales_map.csv

#load packages
library(tidyverse)
library(naniar)
library(janitor)
library(visdat)

#load dataset, also changing a lot of character NA values to actual NA values.
#also clean column names
brooklyn_sales <- read_csv("data/brooklyn_sales_map.csv", na = c("NA", "")) |> 
  clean_names()

#checking some missingness
geom_miss_point(brooklyn_sales)
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

sunrays <- subset(brooklyn_sales, select = -c(borough, 
                                                    mappluto_f, 
                                                    version,
                                                    pluto_map_id,
                                                    x_coord,
                                                    y_coord,
                                                    condo_no,
                                                    bbl,
                                                    boro_code,
                                                    owner_name))

#also may need to redo some parts to check historical districts and see if those are relatively more expensive.


#older code that I may or may or may not use.
brooklyn_sales <- subset(brooklyn_sales, select = -c(borough, Borough, easement, apartment_number))

unique(brooklyn_sales$ZipCode)

brooklyn_sales |> 
  filter(year_of_sale == 2007) |> 
  ggplot(aes(x=gross_sqft, y = sale_price, alpha = 0.8)) + 
  geom_point() + 
  geom_jitter()

