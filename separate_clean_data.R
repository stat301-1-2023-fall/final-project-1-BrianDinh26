#load packages
library(tidyverse)
library(janitor)
#test
#cleaned dataset
brooklyn_sales_clean <- read_csv("data/brooklyn_sales_map.csv", na = c("NA", "")) |> 
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
    school_dist = as.factor(school_dist),
    tax_class_at_sale = as.factor(tax_class_at_sale),
    sale_date = as.Date(sale_date)
  )

write_csv(brooklyn_sales_clean, "data/brooklyn_sales_clean.csv")
