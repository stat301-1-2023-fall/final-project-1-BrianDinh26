#dataset
#https://www.kaggle.com/datasets/tianhwu/brooklynhomes2003to2017/data?select=brooklyn_sales_map.csv

#load packages
library(tidyverse)
library(naniar)
library(janitor)

#load dataset, also changing a lot of character NA values to actual NA values.
#also clean column names
brooklyn_sales <- read_csv("data/brooklyn_sales_map.csv", na = c("NA", "")) |> 
  clean_names()

#checking some missingness
gg_mis_var(brooklyn_sales)




#older code that I may or may or may not use.
brooklyn_sales <- subset(brooklyn_sales, select = -c(borough, Borough, easement, apartment_number))

unique(brooklyn_sales$ZipCode)

brooklyn_sales |> 
  filter(year_of_sale == 2007) |> 
  ggplot(aes(x=gross_sqft, y = sale_price, alpha = 0.8)) + 
  geom_point() + 
  geom_jitter()

