
library(tidyverse)



# loading data ------------------------------------------------------------

data_list_incidence <- list.files(path = "data/unzipdata/incidence/",
                                  pattern = "IHME",
                                  full.names = TRUE)

data_raw_incidence <- data_list_incidence |> 
     map_dfr(read.csv, .id = "source")

data_raw_incidence <- data_raw_incidence |> 
     select(location_id, location_name, sex_name, age_name, year, metric_name, val, upper, lower) |> 
     filter(metric_name %in% c("Number", "Rate"))
     

# panel A -----------------------------------------------------------------

data_panel_A <- data_raw_incidence |> 
     filter(location_name == "Global" & metric_name == "Number" & age_name != "All ages") |>
     group_by(year, age_name) |> 
     summarise(val = sum(val),
               .groups = "drop") |> 
     mutate(age_name = case_when(age_name == "5-9 years" ~ "05-09",
                                 age_name == "5-9 years" ~ "05-09",
                                 age_name == "5-9 years" ~ "05-09",
                                 age_name == "10-14 years" ~ "10-14",
                                 age_name == "15-19 years" ~ "15-19",
                                 TRUE ~ "20+"))

ggplot(data_panel_A, aes(x = year, y = val, fill = age_name)) +
     geom_col(position = "fill") +
     labs(title = "Global incidence of tuberculosis by age group",
          x = "Year",
          y = "Number of cases") +
     theme_minimal() +
     theme(legend.position = "bottom")
