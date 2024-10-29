
library(tidyverse)
library(paletteer)
library(patchwork)
library(Cairo)
library(openxlsx)
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
plot_cfr <- function(i){
     data <- data_region |> 
          filter(location_name == locations[i])
     
     ggplot(data = data) +
          geom_line(aes(x = year, y = CFR, color = age_name)) +
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
                legend.justification = c(1.2, 1),
                legend.position = ifelse(i == 7, 'bottom', 'none'))+
          labs(title = paste0(letters[i+1], ") ", locations[i]),
               x = "Year",
               y = "Case fatality rate (%)",
               color = "Age group") +
          guides(color = guide_legend(nrow = 1,
                                      byrow = TRUE))
}

fig3 <- lapply(2:length(locations), plot_cfr)

fig3 <- wrap_plots(fig3, ncol = 3)

fig2 <- plot_cfr(1)

# all CFR -----------------------------------------------------------------

fill_colors <- paletteer_d("Redmonder::qPBI")

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
                                   levels = locations)) |> 
     arrange(year)

fig1 <- ggplot(data = data_all,
               mapping = aes(x = year, y = CFR, color = location_name)) +
     geom_line() +
     geom_point() +
     scale_x_continuous(breaks = seq(1990, 2021, 5),
                        expand = expansion(mult = c(0.02, 0.45))) +
     scale_y_continuous(breaks = breaks,
                        limits = range(breaks),
                        expand = expansion(mult = c(0, 0))) +
     scale_color_manual(values = fill_colors) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.title.position = 'plot',
           legend.background = element_blank(),
           legend.position = c(1, 0.5),
           legend.justification = c(1, 0.5))+
     labs(title = "a)",
          x = "Year",
          y = "Case fatality rate (%)",
          color = NULL)+
     # reduce the space between legend items
     guides(color = guide_legend(ncol = 1, 
                                 byrow = TRUE,
                                 keyheight = unit(0.4, "cm"),
                                 keywidth = unit(0.4, "cm")))

# save ---------------------------------------------------------------------

design <- "
AAB
CDE
FGH
"

fig <- fig1 + fig2 + fig3[[1]] + fig3[[2]] + fig3[[3]] + fig3[[4]] + fig3[[5]] + fig3[[6]]+
     plot_layout(design = design)

ggsave(filename = "outcome/fig5.pdf",
       plot = fig,
       width = 10,
       height = 8,
       device = cairo_pdf,
       family = "Arial")


write.xlsx(list(panel_a = data_all,
                panel_b_h = data_region),
           file = "outcome/fig5.xlsx",
           asTable = TRUE)

save.image(file = "outcome/fig5.RData")
