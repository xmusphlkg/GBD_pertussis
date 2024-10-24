
library(tidyverse)
library(paletteer)
library(patchwork)
library(Cairo)

# loading data ------------------------------------------------------------

data_raw_death <- read.csv('./data/preparedata/Age_group.csv')

data_raw_death <- data_raw_death |> 
     filter(measure_name == "Deaths" & metric_name %in% c("Number") & year >= 1990) |>
     select(location_id, location_name, sex_name, age_name, year, metric_name, val) |> 
     filter(age_name %in% c('<1 year',  '2-4 years', '5-9 years', '10-14 years', '15-19 years',
                            '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years',
                            '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years',
                            '70-74 years', '75-79 years', '80-84 years', '85-89 years', '90-94 years', '95+ years'))

table(data_raw_death$age_name)

data_clean_death <- data_raw_death |> 
     mutate(Age = case_when(age_name == '<1 year' ~ '0-1',
                            age_name == '95+ years' ~ '95-100',
                            TRUE ~ str_replace_all(age_name, " years", "")),
            StartAge = as.numeric(sub("-.*", "", Age)),
            EndAge = as.numeric(sub(".*-", "", Age)),
            MiddleAge = (StartAge + EndAge) / 2,
            StartAge = if_else(StartAge == 0, 0, StartAge - 0),
            EndAge = if_else(EndAge == 100, 100, EndAge + 0.99)) |> 
     rename(Cases = val) |>
     group_by(location_name, year) |>
     mutate(Weight = Cases/sum(Cases),
            CasesAll = sum(Cases)) |> 
     select(location_name, year, Age, StartAge, EndAge, MiddleAge, Cases, CasesAll, Weight) |> 
     arrange(location_name, year, MiddleAge)

data_clean_age <- data_clean_death |> 
     ungroup() |>
     select(Age, StartAge, EndAge, MiddleAge) |>
     unique() |> 
     arrange(MiddleAge)

# Main --------------------------------------------------------------------

# get median age of each location
data_median_age <- data_clean_death|>
     rowwise() |>
     mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge, 0.01)))) |>
     unnest(cols = c(AgeList)) |>
     filter(!is.na(AgeList)) |>
     group_by(location_name, year, AgeList) |>
     mutate(AverageCases = Cases / ((EndAge - StartAge)*100 + 1)) |>
     ungroup() |>
     select(location_name, year, Age = AgeList, AverageCases) |>
     group_by(location_name, year) |>
     mutate(Weight = AverageCases / sum(AverageCases),
            Weight = case_when(is.na(Weight) ~ 0,
                               TRUE ~ Weight),
            cum_weight = cumsum(Weight)) |>
     summarise(MedianAge = Age[min(which(cum_weight >= 0.5))],
               Q1 = Age[min(which(cum_weight >= 0.25))],
               Q3 = Age[min(which(cum_weight >= 0.75))],
               .groups = 'drop')

locations <- c('Global', 'African Region', 'Eastern Mediterranean Region', 'European Region',
               'Region of the Americas', 'South-East Asia Region', 'Western Pacific Region')
fill_colors <- c(paletteer_d("MoMAColors::OKeeffe"), "#019875FF")
names(fill_colors) <- locations

plot_fun <- function(i){
     location <- locations[i]
     data <- data_median_age |>
          filter(location_name == location) |> 
          mutate(location_name = factor(location_name, levels = locations))
     
     ggplot(data)+
          geom_line(aes(x = year, y = MedianAge, color = location_name))+
          geom_ribbon(aes(x = year, ymin = Q1, ymax = Q3, fill = location_name), alpha = 0.5)+
          scale_x_continuous(breaks = seq(1990, 2021, 5),
                             expand = expansion(mult = c(0.02, 0.02))) +
          scale_y_continuous(breaks = seq(0, 7, 1),
                             limits = c(0, 7),
                             expand = expansion(mult = c(0, 0))) +
          scale_color_manual(values = fill_colors,
                             drop = FALSE)+
          scale_fill_manual(values = fill_colors,
                            drop = FALSE)+
          theme_bw()+
          theme(plot.title.position = "plot",
                legend.direction = "vertical",
                legend.position = "bottom")+
          labs(title = paste0(letters[i], ") ", location),
               x = "Year",
               y = "Median Age of Cases",
               fill = NULL,
               color = NULL)
}

outcome <- lapply(1:length(locations), plot_fun)

outcome[[length(locations) + 1]] <- guide_area()

p <- wrap_plots(outcome, ncol = 2) + plot_layout(guides = "collect")

ggsave(filename = "outcome/fig4.pdf",
       plot = p,
       width = 6,
       height = 10,
       device = cairo_pdf,
       family = "Arial")
