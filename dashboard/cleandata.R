
library(tidyverse)
library(openxlsx)

# rate data ------------------------------------------------------------

data_country_total <- read.csv('./data/preparedata/Country_rate.csv')

data_country_total <- data_country_total |> 
     filter(year >= 1990) |> 
     select(measure_name, location_id, location_name, year, val) |> 
     pivot_wider(names_from = measure_name, values_from = val) |>
     mutate(CFR = (Deaths / Incidence) * 100)

write.csv(data_country_total,
          './dashboard/country.csv',
          row.names = FALSE)