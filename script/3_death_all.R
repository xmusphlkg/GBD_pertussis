
library(tidyverse)
library(paletteer)
library(patchwork)
library(nih.joinpoint)
library(Cairo)

# join point setting
run_opt = run_options(model="ln",
                      max_joinpoints=4,
                      model_selection_method = 'permutation test',
                      ci_method = 'parametric',
                      n_cores=10)
export_opt = export_options()

# loading data ------------------------------------------------------------

data_list_death <- list.files(path = "data/preparedata/",
                              pattern = "GBD",
                              full.names = TRUE)

data_raw_death <- data_list_death |> 
     map_dfr(read.csv)

data_raw_death <- data_raw_death |> 
     filter(measure_name == "Deaths") |>
     select(location_id, location_name, sex_name, age_name, year, metric_name, val, upper, lower) |> 
     filter(metric_name %in% c("Number", "Rate"))


# panel A -----------------------------------------------------------------

data_panel_A <- data_raw_death |> 
     filter(metric_name == "Rate" & age_name == "All ages" & location_name == "Global") |> 
     select(year, val, upper, lower) |> 
     arrange(year)

jp_model <- joinpoint(data_panel_A,
                      year,
                      val,
                      run_opt = run_opt,
                      export_opt = export_opt)
jp_apc <- jp_model$apc

data_panel_A <- data_panel_A |> 
     mutate(jp = as.numeric(jp_model$data_export$model),
            stage = case_when(year >= jp_apc$segment_start[1] & year < jp_apc$segment_end[1] ~ 1,
                              year >= jp_apc$segment_start[2] & year < jp_apc$segment_end[2] ~ 2,
                              year >= jp_apc$segment_start[3] & year < jp_apc$segment_end[3] ~ 3,
                              year >= jp_apc$segment_start[4] & year <= jp_apc$segment_end[4] ~ 4,
                              TRUE ~ 0))

fig1 <- ggplot(data_panel_A, aes(x = year, y = val, group = 'A')) +
     geom_point(mapping = aes(color = 'death')) +
     geom_linerange(aes(ymin = lower, ymax = upper, color = 'death'),
                    show.legend = FALSE) +
     geom_line(mapping = aes(y = jp, color = as.character(stage)))+
     scale_x_continuous(breaks = seq(1990, 2021, 5),
                        expand = expansion(mult = c(0.02, 0.02))) +
     scale_y_continuous(labels = scales::number_format(accuracy = 1),
                        limits = c(0, NA),
                        expand = expansion(mult = c(0, 0.05))) +
     scale_color_manual(values = c(death = "#E84A5FFF",
                                   `1` = "#FECEA8FF",
                                   `2` = "#99B898FF",
                                   `3` = '#FF847CFF',
                                   `4` = "#019875FF"),
                        labels = c(paste0(jp_apc$segment_start[1], '-', jp_apc$segment_end[1], ' APC=', round(jp_apc$apc[1], 2), "*"),
                                   paste0(jp_apc$segment_start[2], '-', jp_apc$segment_end[2], ' APC=', round(jp_apc$apc[2], 2), "*"),
                                   paste0(jp_apc$segment_start[3], '-', jp_apc$segment_end[3], ' APC=', round(jp_apc$apc[3], 2), "*"),
                                   paste0(jp_apc$segment_start[4], '-', jp_apc$segment_end[4], ' APC=', round(jp_apc$apc[4], 2), "*"),
                                   "Mortality rate (95%CI)"))+
     theme_bw()+
     theme(plot.title.position = "plot",
           legend.position = c(0.99, 0.99),
           legend.justification = c(1, 1))+
     labs(title = "a)",
          color = NULL,
          x = "Year",
          y = "Mortality rate, per 100,000")+
     guides(color = guide_legend(override.aes = list(linetype=c(1,1,1,1,1), shape=c(NA,NA,NA,NA,19))))

# panel B -----------------------------------------------------------------

data_panel_B <- data_raw_death |> 
     filter(location_name == "Global" & metric_name == "Number" & age_name != "All ages") |>
     group_by(year, age_name) |> 
     summarise(val = sum(val),
               .groups = "drop") |> 
     mutate(age_name = case_when(age_name == "<1 year" ~ "<01",
                                 age_name == "2-4 years" ~ "02-04",
                                 age_name == "5-9 years" ~ "05-09",
                                 age_name == "10-14 years" ~ "10-14",
                                 age_name == "15-19 years" ~ "15-19",
                                 TRUE ~ "20+"))

fill_colors <- paletteer_d("MoMAColors::OKeeffe")

fig2 <- ggplot(data_panel_B, aes(x = year, y = val, fill = age_name)) +
     geom_col(position = "fill", show.legend = F) +
     geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
     scale_x_continuous(breaks = seq(1990, 2021, 5),
                        expand = c(0, 0)) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                        expand = c(0, 0)) +
     scale_fill_manual(values = fill_colors) +
     theme_bw()+
     theme(plot.title.position = "plot",
           legend.position = "bottom")+
     labs(title = "b)",
          x = "Year",
          y = "Proportion of cases",
          fill = "Age group, years")+
     guides(fill = guide_legend(nrow = 1))


# panel C -----------------------------------------------------------------

data_panel_C <- data_raw_death |> 
     filter(metric_name == "Number" & age_name != "All ages" &
                 location_name %in% c("African Region",
                                      "Region of the Americas",
                                      "South-East Asia Region",
                                      "European Region",
                                      "Eastern Mediterranean Region",
                                      "Western Pacific Region")) |>
     group_by(location_name, year, age_name) |> 
     summarise(val = sum(val),
               .groups = "drop") |> 
     mutate(age_name = case_when(age_name == "<1 year" ~ "<01",
                                 age_name == "2-4 years" ~ "02-04",
                                 age_name == "5-9 years" ~ "05-09",
                                 age_name == "10-14 years" ~ "10-14",
                                 age_name == "15-19 years" ~ "15-19",
                                 TRUE ~ "20+"))

plot_region <- function(i){
     data_panel_C |> 
          filter(location_name == unique(data_panel_C$location_name)[i]) |> 
          ggplot(aes(x = year, y = val, fill = age_name)) +
          geom_col(position = "fill") +
          geom_hline(yintercept = c(0.5, 0.25), linetype = "dashed", color = "black") +
          scale_x_continuous(breaks = seq(1990, 2021, 5),
                             expand = c(0, 0)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                             expand = c(0, 0)) +
          scale_fill_manual(values = fill_colors) +
          theme_bw()+
          theme(plot.title.position = "plot",
                legend.position = "bottom")+
          labs(title = paste0(letters[i+2], ") ", unique(data_panel_C$location_name)[i]), 
               x = "Year",
               y = "death rate, per 100,000",
               fill = "Age group, years")+
          guides(fill = guide_legend(nrow = 1))
}

fig3 <- lapply(1:length(unique(data_panel_C$location_name)), plot_region) |> 
     wrap_plots(nrow = 2, guides = "collect")&
     theme(legend.position = "bottom")

# save --------------------------------------------------------------------

fig12 <- fig1 + fig2 + plot_layout(widths = c(1, 1))

fig <- fig12 / fig3 + plot_layout(heights = c(1.1 , 2))

ggsave(filename = "outcome/fig3.pdf",
       plot = fig,
       width = 12,
       height = 9,
       device = cairo_pdf,
       family = "Arial")
