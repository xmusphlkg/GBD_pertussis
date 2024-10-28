
library(tidyverse)
library(paletteer)
library(patchwork)
library(Cairo)
library(sf)
library(openxlsx)
library(parallel)

# define gaussian kernel
gaussian_kernel <- function(x, mu, sigma) {
     exp(-0.5 * ((x - mu) / sigma)^2) / (sigma * sqrt(2 * pi))
}

get_weights <- function(data, locat){
      data |>
          filter(location_name == locat) |>
          group_by(year) |>
          mutate(Width = EndAge - StartAge,
                 WeightedCases = ifelse(AverageCases > 0, 
                                        sapply(Age, function(age) {
                                             weights <- gaussian_kernel(Age, age, Width) 
                                             sum(weights * AverageCases) / sum(weights)
                                        }),
                                        0)) |>
          mutate(Weight = WeightedCases / sum(WeightedCases),
                 cum_weight = cumsum(Weight))
}

# loading data ------------------------------------------------------------

data_raw_case <- read.csv('./data/preparedata/Region_age.csv')

# filter data with incidence, number, and year >= 1990
data_raw_case <- data_raw_case |> 
     filter(measure_name == "Incidence" & metric_name %in% c("Number") & year >= 1990) |>
     select(location_id, location_name, sex_name, age_name, year, metric_name, val) |> 
     filter(age_name %in% c('<28 days', '1-5 months', '6-11 months', '12-23 months', '2-4 years',
                            '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years',
                            '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years',
                            '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years',
                            '80-84 years', '85-89 years', '90-94 years', '95+ years'))

# check unique age group
table(data_raw_case$age_name)

data_clean_case <- data_raw_case |> 
     mutate(Age = case_when(age_name == '<28 days' ~ '0-1 month',
                            age_name == '95+ years' ~ '95-100 years',
                            TRUE ~ age_name),
            # extract start age, end age, and unit
            ExtractedInfo = str_match(Age, "(\\d+)-(\\d+)\\s*(month|year)"),
            StartAge = as.numeric(ExtractedInfo[, 2]),
            EndAge = as.numeric(ExtractedInfo[, 3]),
            Unit = ExtractedInfo[, 4],
            # fixed overlapping age group for 0-1 month
            EndAge = if_else(StartAge == 0 & Unit == "month", 0.9, EndAge),
            # trans year to month
            StartAge = if_else(Unit == "year", StartAge * 12, StartAge),
            EndAge = if_else(Unit == "year", EndAge * 12, EndAge),
            # calculate middle age
            MiddleAge = (StartAge + EndAge) / 2) |> 
     rename(Cases = val) |>
     group_by(location_name, year) |>
     mutate(Weight = Cases/sum(Cases),
            CasesAll = sum(Cases),
            # replace region name
            location_name = case_when(location_name == "African Region" ~ "Africa",
                                      location_name == "Region of the Americas" ~ "Americas",
                                      location_name == "South-East Asia Region" ~ "South-East Asia",
                                      location_name == "European Region" ~ "Europe",
                                      location_name == "Eastern Mediterranean Region" ~ "Eastern Mediterranean",
                                      location_name == "Western Pacific Region" ~ "Western Pacific",
                                      TRUE ~ location_name)) |>
     select(location_name, year, Age, StartAge, EndAge, MiddleAge, Cases, CasesAll, Weight) |> 
     arrange(location_name, year, MiddleAge)

data_clean_age <- data_clean_case |> 
     ungroup() |>
     select(Age, StartAge, EndAge, MiddleAge) |>
     unique() |> 
     arrange(MiddleAge)

locations <- c('Global', 'Africa', 'Eastern Mediterranean', 'Europe', 'Americas', 'South-East Asia', 'Western Pacific')


# estimate median ---------------------------------------------------------

# Get median age of each location
data_median_age <- data_clean_case |> 
     rowwise() |>
     mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge, 0.1)))) |>
     unnest(cols = c(AgeList)) |>
     filter(!is.na(AgeList)) |>
     group_by(location_name, year, AgeList) |>
     mutate(AverageCases = Cases / ((EndAge - StartAge) * 10 + 1)) |>
     ungroup() |>
     select(location_name, year, Age = AgeList, AverageCases, StartAge, EndAge)

# make cluster to parallel
cl_incidence <- makeCluster(length(locations))

# setting parallel
clusterExport(cl_incidence, c("data_median_age", "locations", "get_weights", "gaussian_kernel"))

# get median age of each location
data_median_age <- parLapply(cl_incidence, locations, function(locat){
     library(dplyr)
     
     get_weights(data_median_age, locat) |>
          group_by(location_name, year) |>
          summarise(MedianAge = Age[min(which(cum_weight >= 0.5))]/12,
                    Q1 = Age[min(which(cum_weight >= 0.25))]/12,
                    Q3 = Age[min(which(cum_weight >= 0.75))]/12,
                    .groups = 'drop')
})

data_median_age <- do.call(rbind, data_median_age)

# stop cluster
stopCluster(cl_incidence)

# visualize median age ----------------------------------------------------
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
               y = "Age of cases",
               fill = "Median (Q1, Q3)",
               color = "Median (Q1, Q3)")
}

outcome <- lapply(1:length(locations), plot_fun)

outcome[[length(locations) + 1]] <- guide_area()

fig1 <- wrap_plots(outcome, ncol = 4) + plot_layout(guides = "collect")

panel_a_g <- data_median_age

# loading map data --------------------------------------------------------

## loading data
data_map <- st_read("./data/world.zh.json") |> 
     filter(iso_a3  != "ATA")
data_map_iso <- read.csv('./data/iso_code.csv')

data_incidence_1990 <- read.csv('./data/preparedata/Country_1990.csv') |> 
     filter(measure_name == "Incidence" & year == 1990)
data_incidence_2019 <- read.csv('./data/preparedata/Country_2019.csv') |> 
     filter(measure_name == "Incidence" & year == 2019)
data_incidence_2021 <- read.csv('./data/preparedata/Country_2021.csv') |> 
     filter(measure_name == "Incidence" & year == 2021)
data_incidence <- rbind(data_incidence_1990, data_incidence_2019, data_incidence_2021) |>
     select(location_id, location_name, year, age_name, val)

remove(data_incidence_1990, data_incidence_2019, data_incidence_2021)

# check all locations in map data
data_location <- data_incidence |> 
     filter(year == 2021) |>
     select(location_id, location_name) |> 
     unique() |> 
     arrange(location_id)

# using newest location name
data_incidence <- data_incidence |> 
     left_join(data_location, by = c("location_id" = "location_id")) |> 
     select(-location_name.x) |> 
     rename(location_name = location_name.y)

# find all countries, year with 0 cases
data_incidence_zero <- data_incidence |> 
     group_by(location_name, year) |> 
     summarise(AllCases = round(sum(val)),
               All = AllCases >= 40,
               .groups = 'drop')

# estimate national median age --------------------------------------------
## estimate median age
data_incidence <- data_incidence |> 
     filter(age_name %in% c('<28 days', '1-5 months', '6-11 months', '12-23 months', '2-4 years',
                            '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years',
                            '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years',
                            '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years',
                            '80-84 years', '85-89 years', '90-94 years', '95+ years')) |> 
     mutate(Age = case_when(age_name == '<28 days' ~ '0-1 month',
                            age_name == '95+ years' ~ '95-100 years',
                            TRUE ~ age_name),
            # extract start age, end age, and unit
            ExtractedInfo = str_match(Age, "(\\d+)-(\\d+)\\s*(month|year)"),
            StartAge = as.numeric(ExtractedInfo[, 2]),
            EndAge = as.numeric(ExtractedInfo[, 3]),
            Unit = ExtractedInfo[, 4],
            # fixed overlapping age group for 0-1 month
            EndAge = if_else(StartAge == 0 & Unit == "month", 0.9, EndAge),
            # trans year to month
            StartAge = if_else(Unit == "year", StartAge * 12, StartAge),
            EndAge = if_else(Unit == "year", EndAge * 12, EndAge),
            # calculate middle age
            MiddleAge = (StartAge + EndAge) / 2) |> 
     left_join(data_incidence_zero, by = c("location_name", "year")) |>
     filter(All) |>
     rename(Cases = val) |>
     group_by(location_name, year) |>
     mutate(Weight = Cases/sum(Cases),
            CasesAll = sum(Cases)) |> 
     select(location_name, year, Age, StartAge, EndAge, MiddleAge, Cases, CasesAll, Weight) |> 
     arrange(location_name, year, MiddleAge)

data_location$location_name[!data_location$location_name %in% unique(data_incidence$location_name)]

locations <- unique(data_incidence$location_name)

data_median_age <- data_incidence |> 
     rowwise() |>
     mutate(AgeList = if_else(is.na(StartAge), list(NA_real_), list(seq(StartAge, EndAge, 0.1))) ) |>
     unnest(cols = c(AgeList)) |>
     filter(!is.na(AgeList)) |>
     group_by(location_name, year, AgeList) |>
     mutate(AverageCases = Cases / ((EndAge - StartAge) * 10 + 1)) |>
     ungroup() |>
     select(location_name, year, Age = AgeList, AverageCases, StartAge, EndAge)

# make cluster to parallel
cl_incidence <- makeCluster(50)

# setting parallel
clusterExport(cl_incidence, c("data_median_age", "locations", "get_weights", "gaussian_kernel"))

# get median age of each location
data_median_age <- parLapply(cl_incidence, locations, function(locat){
     library(dplyr)
     
     get_weights(data_median_age, locat) |>
          group_by(location_name, year) |>
          summarise(MedianAge = Age[min(which(cum_weight >= 0.5))]/12,
                    Q1 = Age[min(which(cum_weight >= 0.25))]/12,
                    Q3 = Age[min(which(cum_weight >= 0.75))]/12,
                    .groups = 'drop')
})

data_median_age <- do.call(rbind, data_median_age)

# stop cluster
stopCluster(cl_incidence)

# visualize median age ----------------------------------------------------
data_median_diff <- data_median_age |> 
     select(location_name, year, MedianAge) |>
     pivot_wider(names_from = year, values_from = MedianAge) |>
     mutate(Diff1 = `2019` - `1990`,
            Diff2 = `2021` - `1990`) |>
     left_join(data_map_iso, by = c("location_name" = "location_name")) |> 
     select(ISO3, location_name, `1990`, `2019`, Diff1, `2021`, Diff2)

panel_h_l <- data_median_diff

## check all locations in map data
data_median_diff[!data_median_diff$ISO3 %in% data_map$iso_a3, ]

## create map plot for 2021
data_map_year <- data_map |> 
     left_join(data_median_diff, by = c("iso_a3" = "ISO3"))

plot_map <- function(i){
     y <- names(data_median_diff)[i+2]
     data <- data_map_year |> 
          select(location_name, geometry, all_of(y)) |> 
          rename(`value` = y)
     
     if (str_detect(y, "Diff")){
          fill_colors <- c("#CAA5C2FF", "#DBC3D6FF", "#F5F5F5FF", "#FFD5C2FF", "#FEC0A3FF", "#FEAB85FF", "#BF714DFF", "#7F4B33FF")
          breaks <- pretty(c(data_median_diff$Diff1, data_median_diff$Diff2), n = 10)
          limits <- range(breaks)
     } else {
          fill_colors <- paletteer_d("Redmonder::dPBIRdGn")
          breaks <- pretty(c(data_median_diff$`1990`, data_median_diff$`2019`, data_median_diff$`2021`), n = 10)
          limits <- range(breaks)
     }
     
     ggplot(data = data) +
          geom_sf(aes(fill = value)) +
          # add x, y tick labels
          theme(axis.text.x = element_text(size = 8),
                axis.text.y = element_text(size = 8)) +
          scale_x_continuous(limits = c(-180, 180),
                           expand = c(0, 0)) + 
          scale_y_continuous(limits = c(-60, 75)) +
          scale_fill_gradientn(colours = fill_colors,
                               limits = limits,
                               breaks = breaks,
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
          labs(title = paste0(letters[i+7], ") "),
               x = NULL,
               y = NULL,
               fill = ifelse(str_detect(y, "Diff"),
                             'Difference of median age',
                             'Median age of cases'))
}

fig2 <- lapply(1:5, plot_map)
fig2 <- list(fig2[[1]], guide_area(), fig2[[2]], fig2[[3]], fig2[[4]], fig2[[5]])

fig2 <- wrap_plots(fig2, ncol = 2) + plot_layout(guides = "collect")

# save --------------------------------------------------------------------

fig <- cowplot::plot_grid(fig1,
                          fig2,
                          nrow = 2,
                          rel_heights = c(1.1, 1.8))

ggsave(filename = "outcome/fig2.pdf",
       plot = fig,
       width = 10,
       height = 12,
       device = cairo_pdf,
       family = "Arial")

write.xlsx(list(panel_a_g = panel_a_g, panel_h_l = panel_h_l),
           "outcome/fig2.xlsx", rowNames = FALSE)

save.image("outcome/fig2.RData")
