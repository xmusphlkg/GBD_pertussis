
library(tidyverse)
library(paletteer)
library(patchwork)
library(Cairo)

# loading data ------------------------------------------------------------

data_raw_case <- read.csv('./data/preparedata/Age_group.csv')

data_raw_case <- data_raw_case |> 
     filter(measure_name == "Incidence" & metric_name %in% c("Number") & year >= 1990) |>
     select(location_id, location_name, sex_name, age_name, year, metric_name, val) |> 
     filter(age_name %in% c('<1 year',  '2-4 years', '5-9 years', '10-14 years', '15-19 years',
                            '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years',
                            '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years',
                            '70-74 years', '75-79 years', '80-84 years', '85-89 years', '90-94 years', '95+ years'))

table(data_raw_case$age_name)

data_clean_case <- data_raw_case |> 
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

data_clean_age <- data_clean_case |> 
     ungroup() |>
     select(Age, StartAge, EndAge, MiddleAge) |>
     unique() |> 
     arrange(MiddleAge)

# # KDE ---------------------------------------------------------------------
# 
# data_clean_kde <- data_clean_case|>
#      rowwise() |>
#      mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge, 0.01)))) |>
#      unnest(cols = c(AgeList)) |>
#      filter(!is.na(AgeList)) |>
#      group_by(location_name, year, AgeList) |>
#      mutate(AverageCases = Cases / ((EndAge - StartAge)*100 + 1)) |>
#      ungroup() |>
#      select(location_name, year, Age = AgeList, AverageCases) |>
#      group_by(location_name, year) |>
#      mutate(
#           Weight = AverageCases / sum(AverageCases),
#           Weight = case_when(
#                is.na(Weight) ~ 0,
#                TRUE ~ Weight
#           )
#      ) |>
#      ungroup() |>
#      group_by(location_name, year) |>
#      summarise(
#           Age_Density = list({
#                dens <- density(Age, weights = Weight,
#                                bw = "SJ",
#                                kernel = "epanechnikov",
#                                adjust = 0.7, from = 0, to = 100)
#                data.frame(Age = dens$x, Density = dens$y)
#           }),
#           .groups = 'drop'
#      ) |>
#      unnest(cols = c(Age_Density))
# 
# # Appendix ----------------------------------------------------------------
# 
# # crate appendix for kde and histogram of each location
# locations <- unique(data_clean_case$location_name)
# 
# plot_fun <- function(i){
#      location <- locations[i]
#      data_his <- data_clean_case |>
#           filter(location_name == location) |> 
#           mutate(Height = Weight / (EndAge - StartAge)) |> 
#           arrange(year, MiddleAge)
#      data_kde <- data_clean_kde |>
#           filter(location_name == location) |> 
#           arrange(year, Age)
#      
#      p1 <- ggplot(data_his) +
#           # add bar
#           geom_rect(aes(xmin = StartAge, xmax = EndAge, ymin = 0, ymax = Height),
#                     fill = "grey", color = "black")+
#           # add kde at second layer
#           geom_path(data = data_kde, aes(x = Age, y = Density),
#                     color = "red")+
#           facet_wrap(~year, scales = "fixed", ncol = 5)+
#           scale_x_continuous(breaks = seq(0, 100, 10),
#                              expand = expansion(mult = c(0, 0)),
#                              limits = c(0, 105))+
#           scale_y_continuous(expand = expansion(mult = c(0, 0.2)))+
#           labs(title = location, x = "Age", y = "Density")
#      
#      ggsave(p1, filename = paste0("./outcome/appendix/", i, "_", location, ".png"),
#             width = 12, height = 12, dpi = 300)
# }
# 
# appendix <- lapply(1:length(locations), plot_fun)
# 
# data_median_age <- data_clean_kde |>
#      group_by(location_name, year) |>
#      arrange(Age) |>
#      mutate(cum_weight = cumsum(Density)) |>
#      summarise(MedianAge = Age[min(which(cum_weight >= sum(Density) / 2))],
#                .groups = 'drop')

# Main --------------------------------------------------------------------

# get median age of each location
data_median_age <- data_clean_case|>
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

ggsave(filename = "outcome/fig2.pdf",
       plot = p,
       width = 6,
       height = 10,
       device = cairo_pdf,
       family = "Arial")
