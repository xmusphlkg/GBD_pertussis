
library(tidyverse)
library(paletteer)
library(patchwork)
library(Cairo)
library(openxlsx)
library(nih.joinpoint)
library(factoextra)
library(sf)

# loading data ------------------------------------------------------------

data_global <- read.csv('./data/preparedata/Region_age.csv') |> 
     filter(year >= 1990)

data_region <- data_global |> 
     filter(age_name %in% c('<1 year', '12-23 months', '2-4 years', '5-9 years', '10-19 years',
                            '20-54 years', '55+ years'),
                 year >= 1990 &
                 metric_name == "Rate") |> 
     select(year, location_name, age_name, measure_name, val) |> 
     pivot_wider(names_from = measure_name, values_from = val) |> 
     mutate(CFR = (Deaths / Incidence) * 100,
            location_name = case_when(location_name == "African Region" ~ "Africa",
                                      location_name == "Region of the Americas" ~ "Americas",
                                      location_name == "South-East Asia Region" ~ "South-East Asia",
                                      location_name == "European Region" ~ "Europe",
                                      location_name == "Eastern Mediterranean Region" ~ "Eastern Mediterranean",
                                      location_name == "Western Pacific Region" ~ "Western Pacific",
                                      TRUE ~ location_name),
            age_name = factor(age_name,
                              levels = c('<1 year', '12-23 months', '2-4 years', '5-9 years', '10-19 years',
                                         '20-54 years', '55+ years')))

# region CFR ---------------------------------------------------------------------

locations <- c('Global', 'Africa', 'Eastern Mediterranean', 'Europe', 'Americas', 'South-East Asia', 'Western Pacific')

fill_colors <- c("#7F312FFF", "#FD817EFF", "#FEC0BFFF", "#99E3DDFF", "#33C6BBFF", "#005C55FF", "#000000FF")
breaks <- pretty(data_region$CFR, n = 5)

# visualizing CFR
plot_cfr <- function(i, l = 'none'){
     data <- data_region |> 
          filter(location_name == locations[i])
     
     ggplot(data = data) +
          geom_line(aes(x = year, y = CFR, color = age_name)) +
          geom_point(aes(x = year, y = CFR, color = age_name)) +
          scale_color_manual(values = fill_colors) +
          scale_x_continuous(breaks = seq(1990, 2021, 5),
                             expand = expansion(mult = c(0.02, 0.02))) +
          scale_y_continuous(breaks = breaks,
                             limits = range(breaks),
                             expand = expansion(mult = c(0, 0))) +
          theme_bw() +
          theme(panel.grid = element_blank(),
                plot.title.position = 'plot',
                legend.background = element_blank(),
                legend.position = l)+
          labs(title = paste0(letters[i+1], ") ", locations[i]),
               x = "Year",
               y = "Case fatality rate (%)",
               color = "Age group") +
          guides(color = guide_legend(nrow = 1,
                                      byrow = TRUE))
}

fig3 <- lapply(2:length(locations), plot_cfr, l = 'bottom')

fig3 <- wrap_plots(fig3, ncol = 3, guides = 'collect') &
     theme(legend.position = 'bottom')

fig2 <- plot_cfr(1)

# all CFR -----------------------------------------------------------------

data_all <- data_global |> 
     filter(age_name == "All ages" & metric_name == "Rate") |>
     select(year, location_name, measure_name, val) |>
     pivot_wider(names_from = measure_name, values_from = val) |>
     mutate(CFR = (Deaths / Incidence) * 100,
            location_name = case_when(location_name == "African Region" ~ "Africa",
                                      location_name == "Region of the Americas" ~ "Americas",
                                      location_name == "South-East Asia Region" ~ "South-East Asia",
                                      location_name == "European Region" ~ "Europe",
                                      location_name == "Eastern Mediterranean Region" ~ "Eastern Mediterranean",
                                      location_name == "Western Pacific Region" ~ "Western Pacific",
                                      TRUE ~ location_name),
            location_name = factor(location_name,
                                   levels = rev(locations))) |> 
     arrange(year)

fig1 <- ggplot(data = data_all,
               mapping = aes(x = year, y = CFR, color = location_name)) +
     geom_line(linetype = 'dashed') +
     geom_point() +
     scale_x_continuous(breaks = seq(1990, 2021, 5),
                        expand = expansion(mult = c(0.02, 0.02))) +
     scale_y_continuous(breaks = breaks,
                        limits = range(breaks),
                        expand = expansion(mult = c(0, 0))) +
     scale_color_manual(values = fill_colors) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.title.position = 'plot',
           legend.background = element_blank(),
           legend.position = c(0.5, 0.99),
           legend.justification = c(0.5, 1))+
     labs(title = "a)",
          x = "Year",
          y = "Case fatality rate (%)",
          color = NULL)+
     guides(color = guide_legend(nrow = 3, 
                                 byrow = TRUE))

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

fig4 <- ggplot(data = data_country_map) +
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
     labs(title = paste0(letters[9], ") "),
          x = NULL,
          y = NULL,
          fill = 'Level of case fatality rate')+
     guides(fill = guide_legend(title.position = 'top'))

# map rate ----------------------------------------------------------------

data_country_years <- data_country |> 
     filter(year %in% c(1990, 2021)) |> 
     as.data.frame() |> 
     mutate(CFR = if_else(CFR > 2, 2, CFR))

plot_year <- function(i){
     data <- data_country_years |>
          filter(year == c(1990, 2021)[i]) |>
          select(location_name, CFR) |>
          left_join(data_map_iso, by = c("location_name" = "location_name"))
     breaks <- pretty(c(data_country_years$CFR), n = 10)
     limits <- range(breaks)
     fill_colors <- rev(paletteer_d("Redmonder::dPBIRdGn"))
     
     data <- data_map |> 
          left_join(data, by = c("iso_a3" = "ISO3"))

     ggplot(data = data) +
          geom_sf(aes(fill = CFR)) +
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
          labs(title = paste0(letters[i + 9], ") "),
               x = NULL,
               y = NULL,
               fill = 'Case fatality rate (%) in 1990 and 2021')
}

fig5 <- plot_year(1)

fig6 <- plot_year(2)


fig_b <- fig4 + guide_area() + fig5 + fig6 +
     plot_layout(guides = "collect", heights = c(1, 1))

fig <- cowplot::plot_grid(fig1 + fig2, fig3, fig_b, ncol = 1, rel_heights = c(1.4, 1.9, 1.8))

ggsave(filename = "outcome/fig5.pdf",
       plot = fig,
       width = 10,
       height = 14,
       device = cairo_pdf,
       family = "Arial")


write.xlsx(list(panel_a = data_all,
                panel_b_h = data_region,
                panel_i = data_country_cfr,
                panel_j_k = data_country_years),
           file = "outcome/fig5.xlsx",
           asTable = TRUE)

save.image(file = "outcome/fig5.RData")