
library(tidyverse)
library(paletteer)
library(patchwork)
library(Cairo)
library(sf)

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

fig1 <- wrap_plots(outcome, ncol = 4) + plot_layout(guides = "collect")

# map ---------------------------------------------------------------------

## loading data

data_map <- st_read("./data/world.zh.json") |> 
     filter(iso_a3  != "ATA")
data_map_iso <- read.csv('./data/iso_code.csv')

data_incidence_1990 <- read.csv('./data/preparedata/Country_1990.csv') |> 
     filter(measure_name == "Incidence" & year == 1990) |>
     select(location_name, year, age_name, val)
data_incidence_2021 <- read.csv('./data/preparedata/Country_2021.csv') |> 
     filter(measure_name == "Incidence" & year == 2021) |>
     select(location_name, year, age_name, val)
data_incidence <- rbind(data_incidence_1990, data_incidence_2021)

remove(data_incidence_1990, data_incidence_2021)

## estimate median age

data_incidence <- data_incidence |> 
     filter(age_name %in% c('<1 year',  '2-4 years', '5-9 years', '10-14 years', '15-19 years',
                            '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years',
                            '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years',
                            '70-74 years', '75-79 years', '80-84 years', '85-89 years', '90-94 years', '95+ years')) |> 
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

data_median_age <- data_incidence |> 
     rowwise() |>
     mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge, 0.01))) ) |>
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
               .groups = 'drop')

data_median_diff <- data_median_age |> 
     pivot_wider(names_from = year, values_from = MedianAge) |>
     mutate(Diff = `2021` - `1990`) |> 
     left_join(data_map_iso, by = c("location_name" = "location_name"))

## check all locations in map data
data_median_diff[!data_median_diff$ISO3 %in% data_map$iso_a3, ]

## create map plot for 2021
data_map_year <- data_map |> 
     left_join(data_median_diff, by = c("iso_a3" = "ISO3"))

fig2 <- ggplot(data = data_map_year) +
     geom_sf(aes(fill = `2021`)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colours = paletteer_d("Redmonder::dPBIRdGn"),
                          limits = c(0, 4),
                          breaks = seq(0, 4, 0.5),
                          na.value = 'grey50')+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#5A98BF50", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           plot.title.position = 'plot',
           legend.position = 'bottom')+
     guides(fill = guide_colorbar(barwidth = 20,
                                  title.position = 'top',
                                  barheight = 1)) +
     labs(title = paste0(letters[length(locations) + 1], ")"),
          x = NULL,
          y = NULL,
          fill = 'Median age of cases in 2021')

## create map plot for diff
fill_colors <- c("#CAA5C2FF", "#DBC3D6FF", "#F5F5F5FF",
                 "#FFD5C2FF", "#FEC0A3FF", "#FEAB85FF", "#BF714DFF", "#7F4B33FF")

fig3 <- ggplot(data = data_map_year) +
     geom_sf(aes(fill = Diff)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colours = fill_colors,
                          limits = c(-0.3, 1.3),
                          breaks = seq(-0.3, 1.3, 0.2),
                          na.value = 'grey50')+
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#5A98BF50", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           plot.title.position = 'plot',
           legend.position = 'bottom')+
     guides(fill = guide_colorbar(barwidth = 20,
                                  title.position = 'top',
                                  barheight = 1)) +
     labs(title = paste0(letters[length(locations) + 2], ")"),
          x = NULL,
          y = NULL,
          fill = 'Difference of median age between 2021 and 1990')

# save --------------------------------------------------------------------

fig <- cowplot::plot_grid(fig1,
                          fig2 | fig3,
                          nrow = 2,
                          rel_heights = c(1, 0.7))

ggsave(filename = "outcome/fig2.pdf",
       plot = fig,
       width = 10,
       height = 8.5,
       device = cairo_pdf,
       family = "Arial")
