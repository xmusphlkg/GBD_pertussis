
library(tidyverse)
library(paletteer)
library(patchwork)
library(Cairo)
library(openxlsx)
library(factoextra)
library(sf)

# country CFR -----------------------------------------------------------------

data_country <- read.csv('./data/preparedata/Country_rate.csv')

data_country <- data_country |> 
     filter(year >= 1990) |> 
     select(measure_name, location_id, location_name, year, val) |> 
     pivot_wider(names_from = measure_name, values_from = val) |>
     mutate(CFR = (Deaths / Incidence) * 100)

# cluster cfr
data_country_cfr <- data_country |> 
     select(location_name, year, CFR) |>
     pivot_wider(names_from = year, values_from = CFR) |> 
     as.data.frame()

rownames(data_country_cfr) <- data_country_cfr$location_name
data_country_cfr <- data_country_cfr |> 
     select(-location_name)

hcdata <- data_country_cfr |> 
     as.matrix() |> 
     scale() |>  
     hkmeans(2)

DataCluster <- hcdata$cluster |>
     as.data.frame() |>
     rename(Cluster = `hcdata$cluster`) |> 
     rownames_to_column(var = "location_name")

data_country_cfr <- data_country_cfr |>
     mutate(median = apply(data_country_cfr, 1, median)) |>
     rownames_to_column(var = "location_name") |>
     left_join(DataCluster, by = 'location_name')

remove(hcdata, DataCluster)

# map cluster ----------------------------------------------------------------

## loading data
data_map <- st_read("./data/world.zh.json") |> 
     filter(iso_a3  != "ATA")
data_map_iso <- read.csv('./data/iso_code.csv')

## merge data
data_country_cfr <- data_country_cfr |> 
     left_join(data_map_iso, by = c("location_name" = "location_name")) |> 
     mutate(Cluster = factor(Cluster,
                             levels = c(1, 2),
                             labels = c("Low CFR", "High CFR"))) |> 
     select(location_name, Cluster) |> 
     left_join(data_map_iso, by = c("location_name" = "location_name"))

data_country_map <- data_map |> 
     left_join(data_country_cfr, by = c("iso_a3" = "ISO3"))

fig3 <- ggplot(data = data_country_map) +
     geom_sf(aes(fill = Cluster)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_manual(values = c("#5773CCFF", "#FFB900FF"),
                       na.value = "grey50",
                       na.translate = FALSE) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#5A98BF50", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           plot.title.position = 'plot',
           legend.position = 'bottom')+
     labs(title = paste0(letters[3], ") "),
          x = NULL,
          y = NULL,
          fill = 'Level of case fatality rate')+
     guides(fill = guide_legend(title.position = 'top'))

# map rate ----------------------------------------------------------------

data_country_years <- data_country |> 
     filter(year %in% c(1990, 2021)) |> 
     as.data.frame() |> 
     mutate(CFR_l = if_else(CFR > 2, 2, CFR))

plot_year <- function(i){
     data <- data_country_years |>
          filter(year == c(1990, 2021)[i]) |>
          select(location_name, CFR_l) |>
          left_join(data_map_iso, by = c("location_name" = "location_name"))
     breaks <- pretty(c(data_country_years$CFR_l), n = 10)
     limits <- range(breaks)
     fill_colors <- rev(paletteer_d("Redmonder::dPBIRdGn"))
     
     data <- data_map |> 
          left_join(data, by = c("iso_a3" = "ISO3"))

     ggplot(data = data) +
          geom_sf(aes(fill = CFR_l)) +
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
          labs(title = paste0(letters[i], ") "),
               x = NULL,
               y = NULL,
               fill = 'Case fatality rate (%) in 1990 and 2021')
}

fig1 <- plot_year(1)

fig2 <- plot_year(2)

fig <- fig1 + fig2 + fig3 + guide_area() +
     plot_layout(guides = "collect", heights = c(1, 1))

ggsave(filename = "outcome/fig6.pdf",
       plot = fig,
       width = 10,
       height = 5,
       device = cairo_pdf,
       family = "Arial")

write.xlsx(list(panel_a = data_country_cfr,
                panel_b_c = data_country_years),
           file = "outcome/fig6.xlsx",
           asTable = TRUE)

save.image(file = "outcome/fig6.RData")
