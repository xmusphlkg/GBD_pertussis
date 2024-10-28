
library(tidyverse)
library(paletteer)
library(patchwork)
library(Cairo)
library(openxlsx)
library(factoextra)

# loading data ------------------------------------------------------------

data_global <- read.csv('./data/preparedata/Region_age.csv')

data_global <- data_global |> 
     filter(age_name %in% c('<28 days', '1-5 months', '6-11 months', '12-23 months', '2-4 years',
                            '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years',
                            '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years',
                            '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years',
                            '80-84 years', '85-89 years', '90-94 years', '95+ years', 'All ages') &
                 year >= 1990 &
                 metric_name == "Rate") |> 
     mutate(age_name = case_when(age_name %in% c('<28 days', '1-5 months', '6-11 months') ~ "[0,1)",
                                 age_name %in% c('12-23 months') ~ "[1,2)",
                                 age_name %in% c('2-4 years', '5-9 years') ~ "[2,9)",
                                 age_name %in% c('10-14 years', '15-19 years') ~ "[10,19)",
                                 TRUE ~ "[20,)"),
            age_name = factor(age_name, levels = c("[0,1)", "[1,2)", "[2,9)", "[10,19)", "[20,)"))) |> 
     group_by(year, location_name, age_name, measure_name) |>
     select(year, location_name, age_name, measure_name, val) |> 
     pivot_wider(names_from = measure_name, values_from = val)




data_raw_all <- read.csv('./data/preparedata/Country_rate.csv')

data_clean_all <- data_raw_all |> 
     filter(year >= 1990) |> 
     select(measure_name, location_id, location_name, year, val)

remove(data_raw_all)

# replace location_name woth new names
data_location <- data_clean_all |> 
     filter(year == 2021) |>
     select(location_id, location_name) |> 
     unique() |> 
     arrange(location_id)

data_clean_all <- data_clean_all |>
     left_join(data_location, by = 'location_id') |>
     select(-location_name.x) |>
     rename(location_name = location_name.y)

remove(data_location)

# split data into incidence and mortality
data_clean_incidence <- data_clean_all |> 
     filter(measure_name == 'Incidence') |> 
     select(-measure_name) |> 
     pivot_wider(names_from = year, values_from = val) |> 
     arrange(location_id) |> 
     select(-location_id) |> 
     as.data.frame()

data_clean_mortality <- data_clean_all |>
     filter(measure_name == 'Deaths') |>
     select(-measure_name) |> 
     pivot_wider(names_from = year, values_from = val) |>
     arrange(location_id) |>
     select(-location_id) |> 
     as.data.frame()

# cluster -----------------------------------------------------------------

# cluster incidence
rownames(data_clean_incidence) <- data_clean_incidence$location_name
data_clean_incidence <- data_clean_incidence |> 
     select(-location_name)

hcdata <- data_clean_incidence |> 
     as.matrix() |> 
     scale() |>  
     hkmeans(2)

DataCluster <- hcdata$cluster |>
     as.data.frame() |>
     rename(Cluster = `hcdata$cluster`) |> 
     rownames_to_column(var = "location_name")

data_clean_incidence <- data_clean_incidence |>
     mutate(median = apply(data_clean_incidence, 1, median)) |>
     rownames_to_column(var = "location_name") |>
     left_join(DataCluster, by = 'location_name')

remove(hcdata, DataCluster)

# cluster mortality
rownames(data_clean_mortality) <- data_clean_mortality$location_name
data_clean_mortality <- data_clean_mortality |> 
     select(-location_name)

hcdata <- data_clean_mortality |>
     as.matrix() |>
     scale() |>
     hkmeans(2)

DataCluster <- hcdata$cluster |>
     as.data.frame() |>
     rename(Cluster = `hcdata$cluster`) |>
     rownames_to_column(var = "location_name")

data_clean_mortality <- data_clean_mortality |>
     mutate(median = apply(data_clean_mortality, 1, median)) |>
     rownames_to_column(var = "location_name") |>
     left_join(DataCluster, by = 'location_name')

remove(hcdata, DataCluster)

# cluster data
data_cluster <- data_clean_incidence |>
     select(location_name, Cluster) |>
     left_join(data_clean_mortality |>
                   select(location_name, Cluster), by = 'location_name') |>
     rename(Cluster_incidence = Cluster.x, Cluster_mortality = Cluster.y)

# map cluster ----------------------------------------------------------------

## loading data
data_map <- st_read("./data/world.zh.json") |> 
     filter(iso_a3  != "ATA")
data_map_iso <- read.csv('./data/iso_code.csv')

## merge data
data_cluster <- data_cluster |> 
     left_join(data_map_iso, by = c("location_name" = "location_name")) |> 
     mutate(Cluster_incidence = factor(Cluster_incidence,
                                       levels = c(1, 2),
                                       labels = c("Low", "High")),
            Cluster_mortality = factor(Cluster_mortality,
                                       levels = c(1, 2),
                                       labels = c("Low", "High")))
# data_cluster <- data_map |> 
#      left_join(data_cluster, by = c("iso_a3" = "ISO3"))

plot_cluster <- function(i){
     if (i == 1){
          labels <- c("Low incidence", "High incidence")
          value <- "Cluster_incidence"
     } else {
          labels <- c("Low mortality", "High mortality")
          value <- "Cluster_mortality"
     }
     data <- data_cluster |> 
          select(ISO3, all_of(value)) |> 
          rename(value = all_of(value))
     data <- data_map |> 
          left_join(data, by = c("iso_a3" = "ISO3"))
     
     ggplot(data = data) +
          geom_sf(aes(fill = value)) +
          # add x, y tick labels
          theme(axis.text.x = element_text(size = 8),
                axis.text.y = element_text(size = 8)) +
          scale_x_continuous(limits = c(-180, 180),
                             expand = c(0, 0)) + 
          scale_y_continuous(limits = c(-60, 75)) +
          scale_fill_manual(values = c("#5773CCFF", "#FFB900FF"),
                            labels = labels,
                            na.value = "grey50",
                            na.translate = FALSE) +
          theme_bw() +
          theme(panel.grid = element_blank(),
                panel.background = element_rect(fill = "#5A98BF50", color = NA),
                axis.text = element_text(color = 'black', face = 'plain'),
                axis.title = element_text(color = 'black', face = 'plain'),
                plot.title.position = 'plot',
                legend.background = element_rect(fill = NA, color = NA),
                legend.position = c(0, 0),
                legend.justification = c(0, 0))+
          labs(title = paste0(letters[i], ") "),
               x = NULL,
               y = NULL,
               fill = NULL)
}

fig1 <- lapply(1:2, plot_cluster) |> 
     wrap_plots(nrow = 1)

# map rate ----------------------------------------------------------------

plot_year <- function(i, index){
     y <- c(1990, 2019, 2021)[i]
     if (index == 'incidence'){
          data <- data_clean_incidence |>
               select(location_name, as.character(y)) |>
               left_join(data_map_iso, by = c("location_name" = "location_name")) |>
               rename(value = as.character(y))
          breaks <- pretty(c(data_clean_incidence$`1990`, data_clean_incidence$`2019`, data_clean_incidence$`2021`), n = 10)
          limits <- range(breaks)
          fill_colors <- rev(paletteer_d("Redmonder::dPBIRdGn"))
          l <- i*2+1
     } else {
          data <- data_clean_mortality |>
               select(location_name, as.character(y)) |>
               left_join(data_map_iso, by = c("location_name" = "location_name")) |>
               rename(value = as.character(y))
          breaks <- pretty(c(data_clean_mortality$`1990`, data_clean_mortality$`2019`, data_clean_mortality$`2021`), n = 10)
          limits <- range(breaks)
          fill_colors <- paletteer_d("Redmonder::dPBIPuOr")
          l <- i*2+2
     }
     print(paste0("Year: ", y, " Index: ", index, ' l:', l))
     
     data <- data_map |> 
          left_join(data, by = c("iso_a3" = "ISO3"))

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
          labs(title = paste0(letters[l], ") "),
               x = NULL,
               y = NULL,
               fill = NULL)
}

fig2 <- lapply(1:3, plot_year, index = 'incidence') |> 
     wrap_plots(ncol = 1, guides = 'collect')&
     theme(legend.position = 'bottom')

fig3 <- lapply(1:3, plot_year, index = 'mortality') |>
     wrap_plots(ncol = 1, guides = 'collect')&
     theme(legend.position = 'bottom')

fig <- cowplot::plot_grid(fig1,
                          fig2 | fig3,
                          ncol = 1, rel_heights = c(0.5, 1.6))

ggsave(filename = "outcome/fig5.pdf",
       plot = fig,
       width = 10,
       height = 11,
       device = cairo_pdf,
       family = "Arial")


write.xlsx(list(panel_a_b = data_cluster,
                panel_c_e_g = data_clean_incidence,
                panel_d_f_h = data_clean_mortality),
           "outcome/fig5.xlsx", rowNames = FALSE)
