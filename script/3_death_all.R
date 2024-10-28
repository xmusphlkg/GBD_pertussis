
library(tidyverse)
library(paletteer)
library(patchwork)
library(nih.joinpoint)
library(Cairo)
library(openxlsx)

# join point setting
run_opt = run_options(model="ln",
                      max_joinpoints=4,
                      model_selection_method = 'permutation test',
                      ci_method = 'parametric',
                      n_cores=10)
export_opt = export_options()

# loading data ------------------------------------------------------------

data_global <- read.csv('./data/preparedata/Region_age.csv')

data_global <- data_global |> 
     filter(age_name %in% c('<28 days', '1-5 months', '6-11 months', '12-23 months', '2-4 years',
                            '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years',
                            '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years',
                            '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years',
                            '80-84 years', '85-89 years', '90-94 years', '95+ years', 'All ages') &
                 year >= 1990 &
                 measure_name == "Deaths")

# trend data for global incidence
data_trend <- data_global |> 
     filter(location_name == "Global" & age_name == "All ages") |>
     select(location_id, location_name, sex_name, age_name, year, metric_name, val, upper, lower) |> 
     filter(metric_name %in% c("Number", "Rate")) |> 
     arrange(year)

data_trend_number <- data_trend |> 
     filter(metric_name == "Number") |> 
     select(year, val, lower, upper)

data_trend_rate <- data_trend |>
     filter(metric_name == "Rate") |> 
     select(year, val, lower, upper)

cat("Global deaths: ", sum(data_trend_number$val), "\n")

# age group trend data for global incidence
data_adult <- data_global |> 
     filter(location_name == "Global" & metric_name == "Number" & age_name != "All ages") |>
     select(location_id, location_name, sex_name, age_name, year, metric_name, val) |> 
     mutate(adult = !age_name %in% c('<28 days', '1-5 months', '6-11 months', '12-23 months', '2-4 years',
                                     '5-9 years', '10-14 years', '15-19 years')) |> 
     group_by(adult) |> 
     summarise(val = sum(val),
               .groups = "drop")

cat("Adult incidence: ", data_adult$val[data_adult$adult == TRUE],
    data_adult$val[data_adult$adult == TRUE]/sum(data_adult$val), "\n")

cat("Child incidence: ", data_adult$val[data_adult$adult == FALSE],
    data_adult$val[data_adult$adult == FALSE]/sum(data_adult$val), "\n")

# panel A -----------------------------------------------------------------

data_panel_A <- data_trend_rate

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
                              TRUE ~ 0))

cat("Joinpoint APC for mortality rate: ", jp_apc$apc, "\n")
print(jp_apc)

cat("Joinpoint for mortality rate: ", "\n")
print(data_panel_A[data_panel_A$year %in% c(jp_apc$segment_start, jp_apc$segment_end),])

cat("Joinpoint for deaths: ", "\n")
print(data_trend_number[data_trend_number$year %in% c(jp_apc$segment_start, jp_apc$segment_end),])

cat("Decrease in deaths: ", data_trend_number$val[data_trend_number$year == 2021] - data_trend_number$val[data_trend_number$year == 1990],
    (data_trend_number$val[data_trend_number$year == 2021] - data_trend_number$val[data_trend_number$year == 1990])/data_trend_number$val[data_trend_number$year == 1990],
    "\n")

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
                                   `2` = "#99B898FF"),
                        labels = c(paste0(jp_apc$segment_start[1], '-', jp_apc$segment_end[1], ' APC=', round(jp_apc$apc[1], 2), "*"),
                                   paste0(jp_apc$segment_start[2], '-', jp_apc$segment_end[2], ' APC=', round(jp_apc$apc[2], 2), "*"),
                                   "Mortality rate (95%CI)"))+
     theme_bw()+
     theme(plot.title.position = "plot",
           legend.position = c(0.99, 0.99),
           legend.justification = c(1, 1))+
     labs(title = "a)",
          color = NULL,
          x = "Year",
          y = "Mortality rate, per 100,000")+
     guides(color = guide_legend(override.aes = list(linetype=c(1,1,1), shape=c(NA,NA,19))))

# panel B -----------------------------------------------------------------

data_panel_B <- data_global |> 
     filter(location_name == "Global" & metric_name == "Number" & age_name != "All ages") |>
     mutate(age_name = case_when(age_name %in% c('<28 days', '1-5 months', '6-11 months') ~ "[0,1)",
                                 age_name %in% c('12-23 months') ~ "[1,2)",
                                 age_name %in% c('2-4 years', '5-9 years') ~ "[2,9)",
                                 age_name %in% c('10-14 years', '15-19 years') ~ "[10,19)",
                                 TRUE ~ "[20,)"),
            age_name = factor(age_name, levels = c("[0,1)", "[1,2)", "[2,9)", "[10,19)", "[20,)"))) |>
     group_by(year, age_name) |> 
     summarise(val = sum(val),
               .groups = "drop") |> 
     group_by(year) |> 
     mutate(val = val/sum(val))

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

data_panel_C <- data_global |> 
     filter(location_name != "Global" & metric_name == "Number" & age_name != "All ages") |> 
     mutate(age_name = case_when(age_name %in% c('<28 days', '1-5 months', '6-11 months') ~ "[0,1)",
                                 age_name %in% c('12-23 months') ~ "[1,2)",
                                 age_name %in% c('2-4 years', '5-9 years') ~ "[2,9)",
                                 age_name %in% c('10-14 years', '15-19 years') ~ "[10,19)",
                                 TRUE ~ "[20,)"),
            age_name = factor(age_name, levels = c("[0,1)", "[1,2)", "[2,9)", "[10,19)", "[20,)")),
            # replace region name
            location_name = case_when(location_name == "African Region" ~ "Africa",
                                      location_name == "Region of the Americas" ~ "Americas",
                                      location_name == "South-East Asia Region" ~ "South-East Asia",
                                      location_name == "European Region" ~ "Europe",
                                      location_name == "Eastern Mediterranean Region" ~ "Eastern Mediterranean",
                                      location_name == "Western Pacific Region" ~ "Western Pacific")) |> 
     group_by(location_name, year, age_name) |> 
     summarise(val = sum(val),
               .groups = "drop") |>
     group_by(location_name, year) |>
     mutate(val_prop = round(100 * val/sum(val), 2))

data_sum <- data_panel_C |> 
     filter(year %in% c(1990, 2021)) |> 
     pivot_wider(id_cols = c("location_name", "age_name"),
                 names_from = "year",
                 values_from = "val_prop") |> 
     mutate(diff = `2021` - `1990`)

data_sum <- data_panel_C |> 
     pivot_wider(id_cols = c("location_name", "age_name"),
                 names_from = "year",
                 values_from = "val_prop") |> 
     arrange(location_name, age_name)

locations <- c('Africa', 'Eastern Mediterranean', 'Europe', 'Americas', 'South-East Asia', 'Western Pacific')

plot_region <- function(i){
     data_panel_C |> 
          filter(location_name == locations[i]) |> 
          ggplot(aes(x = year, y = val, fill = age_name)) +
          geom_col(position = "fill") +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
          scale_x_continuous(breaks = seq(1990, 2021, 5),
                             expand = c(0, 0)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                             expand = c(0, 0)) +
          scale_fill_manual(values = fill_colors) +
          theme_bw()+
          theme(plot.title.position = "plot",
                legend.position = "bottom")+
          labs(title = paste0(letters[i+2], ") ", locations[i]), 
               x = "Year",
               y = "death rate, per 100,000",
               fill = "Age group, years")+
          guides(fill = guide_legend(nrow = 1))
}

fig3 <- lapply(1:length(locations), plot_region) |> 
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

write.xlsx(list(data_panel_A, data_panel_B, data_panel_C),
           file = "outcome/fig3.xlsx",
           asTable = TRUE,
           rowNames = FALSE)

save.image("outcome/fig3.RData")