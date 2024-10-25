
library(shinyBS)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(openxlsx)
library(datamods)

load("data.RData")

# Define UI
ui <- fluidPage(
     title = 'Estimate age distribution',
     tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"),
          tags$style(HTML(".tooltip-inner {max-width: 300px; text-align: left;}"))
     ),
     tags$h1("Estimate age distribution", align = "center"),
     setBackgroundColor(color = "ghostwhite"),
     useShinydashboard(),
     
     ## data import--------------------------------------------------------------
     box(
          title = 'Step 1: Import Data',
          width = 12,
          collapsible = T,
          status = 'danger',
          box(
               width = 6,
               title = 'Data Source',
               status = 'danger',
               import_ui(
                    id = 'InputData',
                    from = c("env", "file", "copypaste", "googlesheets", "url"),
                    file_extensions = c(".csv", ".txt", ".xlsx")
               ),
               footer = tags$span("Example data were provided in environment, and you can also upload your own data from ",
                             tags$a("GBD 2021", href = "https://vizhub.healthdata.org/gbd-results/"))
          ),
          box(
               width = 6,
               title = 'Data Settings',
               status = 'danger',
               tags$b("Import status:"),
               verbatimTextOutput(outputId = "status"),
               tags$b("Name:"),
               verbatimTextOutput(outputId = "name"),
               tags$b("Data:"),
               verbatimTextOutput(outputId = "data")
          )
     ),
     box(
          title = 'Step 2: Filter Data',
          width = 12,
          collapsible = T,
          status = 'danger',
          box(
               width = 6,
               title = 'Data Filter',
               status = 'danger',
               uiOutput(
                    outputId = "filter_ui",
                    inline = T
               )
          ),
          box(
               width = 6,
               title = 'Data Settings',
               status = 'danger',
               tags$b("Data Filter Summary:"),
               verbatimTextOutput(outputId = "data_filter_summary"),
               tags$b("Date Transition:"),
               verbatimTextOutput(outputId = "data_age"),
               tags$b("Date Check:"),
               verbatimTextOutput(outputId = "data_check")
          )
     ),
     ## median -----------------------------
     box(
          title = 'Step 3: Estimation Logs',
          width = 12,
          collapsible = T,
          status = 'danger',
          conditionalPanel(
               condition = "output.data_filter_summary",
               column(
                    12,
                    actionButton(
                         inputId = "start_estimate",
                         label = "Start Estimation",
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;"
                    )
               ),
               column(
                    12,
                    tags$div(
                         tags$h4("Estimation Logs"),
                         verbatimTextOutput(outputId = "log_output"))
               )
          )
     ),
     box(
          title = 'Step 4: Outcome',
          width = 12,
          collapsible = T,
          status = 'danger',
          conditionalPanel(
               condition = "output.data_filter_summary",
               column(
                    12,
                    plotOutput(
                         outputId = "outcome_1",
                         width = "100%"
                    )
               ),
               column(
                    12,
                    downloadButton(
                         "downloadData",
                         "Download"
                    )
               )
          )
     ),
     # add footer
     column(
          width = 12,
          tags$footer(
               tags$p(
                    "Author: Kangguo Li",
                    tags$br(),
                    tags$em("Version 1.0.0"),
                    tags$br(),
                    tags$em("Last updated: 2024-10-25"),
                    tags$br(),
                    tags$em("Host: shinyapps.io"),
                    tags$br(),
                    tags$a(
                         href = "https://github.com/xmusphlkg/GBD_pertussis",
                         icon("github"),
                         title = "Source Code",
                         style = "margin: 10px"
                    ),
                    style = "text-align: center; color: #777; font-size: 12px; margin-top: 10px;"
               )
          )
     )
)

# Define server
server <- function(input, output, session) {
     
     # global values
     GlobalData <- reactiveValues(
          data = NULL,
          data_filter = NULL,
          data_median = NULL,
          logs = "## Start estimation"
     )
     
     ## import data--------------------------------------------------------------
     
     imported <- import_server(
          id = "InputData",
          return_class = c("data.frame", "data.table", "tbl_df", "raw"),
          read_fns = list(
               xlsx = function(file, sheet, skip, encoding) {
                    openxlsx::read.xlsx(path = file, sheet = sheet, startRow = skip + 1, detectDates = T)
               },
               csv = function(file, sheet, skip, encoding) {
                    read.csv(file, skip = skip, encoding = encoding)
               }
          )
     )

     observeEvent(imported$name(), {
          output$status <- renderPrint({
               Data <- imported$data()
               # check data is not null
               if (is.null(Data)) {
                    return("Import failed, because data is NULL")
               }
               # check data is not empty
               if (nrow(Data) == 0) {
                    return("Import failed, because data is empty")
               }
               # check data contains 'year' column
               if (!("year" %in% colnames(Data))) {
                    return("Import failed, because data does not contain 'year' column")
               }
               # check data contains 'val' column
               if (!("val" %in% colnames(Data))) {
                    return("Import failed, because data does not contain 'val' column")
               }
               # check data contains 'age_name' column
               if (!("age_name" %in% colnames(Data))) {
                    return("Import failed, because data does not contain 'age_name' column")
               }
               # check 'year' column is integer type
               if (!is.integer(Data$year)) {
                    return("Import failed, because 'date' column is not date type")
               }
               # check 'val' column is numeric or integer type
               if (!is.numeric(Data$val)) {
                    return("Import failed, because 'val' column is not numeric or integer type")
               }
               GlobalData$data <- Data |>
                    arrange(year)
               return("Import success")
          })
          output$name <- renderPrint({
               imported$name()
          })
          output$data <- renderPrint({
               head(imported$data())
          })
     })
     
     ## filter data ------------------------------------------------------------

     # filter data
     observeEvent(GlobalData$data, {
          Data <- GlobalData$data
          location_name <- unique(Data$location_name)
          sex_name <- unique(Data$sex_name)
          age_name <- unique(Data$age_name)
          measure_name <- unique(Data$measure_name)

          output$filter_ui <- renderUI({
               tags$div(
                    column(
                         6,
                         selectInput(
                              inputId = "data_location_name",
                              label = "Location Name",
                              selected = ifelse(length(location_name) > 5,
                                               location_name[1:5],
                                               location_name),
                              choices = location_name,
                              multiple = T
                         )
                    ),
                    column(
                         6,
                         selectInput(
                              inputId = "data_sex_name",
                              label = "Sex Name",
                              selected = ifelse(length(sex_name) > 5,
                                               sex_name[1:5],
                                               sex_name),
                              choices = sex_name,
                              multiple = T
                         )
                    ),
                    column(
                         6,
                         selectInput(
                              inputId = "data_age_name",
                              label = "Age Name",
                              selected = ifelse(length(age_name) > 5,
                                               age_name[1:5],
                                               age_name),
                              choices = age_name,
                              multiple = T
                         )
                    ),
                    column(
                         6,
                         selectInput(
                              inputId = "data_measure_name",
                              label = "Measure Name",
                              choices = unique(Data$measure_name),
                              selected = unique(Data$measure_name)[1],
                              multiple = F
                         )
                    ),
                    column(
                         6,
                         sliderInput(
                              inputId = "data_year_range",
                              label = "Year Range",
                              min = min(Data$year),
                              max = max(Data$year),
                              value = c(min(Data$year), max(Data$year)),
                              step = 1
                         )
                    ),
                    column(
                         6,
                         sliderInput(
                              inputId = "data_age_digits",
                              label = HTML("Number of Digits <i id='info-age-digits' class='fa fa-info-circle'></i>"),
                              min = 0,
                              max = 4,
                              value = 2,
                              step = 1
                         ),
                         bsTooltip(id = "info-age-digits",
                                   title = "Number of digits to round the age, the more digits, the more accurate the age and the more time it takes to calculate",
                                   placement = "right", trigger = "hover")
                    ),
                    column(
                         2,
                         actionButton(
                              inputId = "filter_data",
                              label = "Split",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 25px;"
                         )
                    ),
               )
          })
     })
     
     ## add start and end year --------------------------
     observeEvent(input$filter_data, {
          Data <- GlobalData$data
          number_of_digits <- input$data_age_digits
          data_filter <- Data  |> 
               filter(location_name %in% input$data_location_name &
                           sex_name %in% input$data_sex_name &
                           age_name %in% input$data_age_name &
                           measure_name == input$data_measure_name &
                           year >= input$data_year_range[1] &
                           year <= input$data_year_range[2])
          
          data_filter_years <- data_filter  |> 
               filter(str_detect(age_name, "year"))
          if (nrow(data_filter_years) > 0) {
               data_filter_years <- data_filter_years |>
                    mutate(Age = str_replace_all(age_name, "<", "0-"),
                           Age = str_replace_all(Age, "\\+ years", "-100 years"),
                           Age = str_replace_all(Age, " years", ""),
                           StartAge = as.numeric(sub("-.*", "", Age)),
                           EndAge = as.numeric(sub(".*-", "", Age)),
                           MiddleAge = (StartAge + EndAge) / 2,
                           EndAge = if_else(str_detect(age_name, "<"), EndAge-1, EndAge),
                           EndAge = if_else(EndAge == 100, 100, EndAge + 1 - 10^(-number_of_digits)))
          } else {
               # using empty data frame to keep the same structure
               data_filter_years <- data_filter[0, ]
          }
          
          
          data_filter_months <- data_filter  |> 
               filter(str_detect(age_name, "month"))
          if (nrow(data_filter_months) > 0) {
               data_filter_months <- data_filter_months |>
                    mutate(Age = str_replace_all(age_name, "<", "0-"),
                           Age = str_replace_all(Age, " months", ""),
                           StartAge = as.numeric(sub("-.*", "", Age)),
                           EndAge = as.numeric(sub(".*-", "", Age)),
                           MiddleAge = (StartAge + EndAge) / 2,
                           EndAge = if_else(str_detect(age_name, "<"), EndAge-1, EndAge),
                           EndAge = if_else(EndAge == 100, 100, EndAge + 1 - 10^(-number_of_digits)),
                           StartAge = StartAge / 12,
                           EndAge = EndAge / 12,
                           MiddleAge = MiddleAge / 12)
          } else {
               data_filter_months <- data_filter[0, ]
          }
          
          data_filter_days <- data_filter  |>
               filter(str_detect(age_name, "day"))
          if (nrow(data_filter_days) > 0) {
               data_filter_days <- data_filter_days |>
                    mutate(Age = str_replace_all(age_name, "<", "0-"),
                           Age = str_replace_all(Age, " days", ""),
                           StartAge = as.numeric(sub("-.*", "", Age)),
                           EndAge = as.numeric(sub(".*-", "", Age)),
                           MiddleAge = (StartAge + EndAge) / 2,
                           EndAge = if_else(str_detect(age_name, "<"), EndAge-1, EndAge),
                           EndAge = if_else(EndAge == 100, 100, EndAge + 1 - 10^(-number_of_digits)),
                           StartAge = StartAge / 365.25,
                           EndAge = EndAge / 365.25,
                           MiddleAge = MiddleAge / 365.25)
          } else {
               data_filter_days <- data_filter[0, ]
          }
          
          data_filter <- rbind(data_filter_years, data_filter_months, data_filter_days)
          
          data_date <- data_filter |> 
               select(age_name, Age, StartAge, EndAge, MiddleAge) |> 
               unique() |> 
               arrange(StartAge, EndAge) |> 
               mutate(Overlaps = if_else(StartAge < lag(EndAge, default = 0), 1, 0))
          
          output$data_filter_summary <- renderPrint({
               data_filter |> 
                    select(location_name, sex_name, age_name, measure_name, year, val) |> 
                    str()
          })
          
          output$data_age <- renderPrint({
               data_date
          })
          
          ## check the range of age is overlapped
          output$data_check <- renderPrint({
               data_overlaps <- any(data_date$Overlaps == 1)
               if (data_overlaps) {
                    GlobalData$data_filter <- NULL
                    showNotification(ui = "Age range is overlapped, please check the data and filter again",
                                     type = "error",
                                     duration = 60)
                    print("Age range is overlapped, please check the data and filter again")
               } else {
                    GlobalData$data_filter <- data_filter
                    showNotification(ui = "Age range is not overlapped, data is ready for analysis",
                                     type = "default",
                                     duration = 5)
                    print("Age range is not overlapped, data is ready for analysis")
               }
          })
     })
     
     ## Median ---------------------------------------------------------------------
     
     output$log_output <- renderText({
          GlobalData$logs
     })

     observeEvent(input$start_estimate, {
          Data <- GlobalData$data_filter
          data_age_digits <- input$data_age_digits
          
          current_logs <- GlobalData$logs
          new_log <- paste(Sys.time(), "- Start to estimate median age")
          GlobalData$logs <- paste(current_logs, new_log, sep = "\n")
          
          # forwards to find median of each group
          Data <- Data |> 
               rowwise() |>
               rename(Cases = val) |>
               mutate(AgeList = if_else(is.na(StartAge), list(NA_real_),
                                        list(seq(StartAge, EndAge, 10^(-input$data_age_digits))))) |> 
               unnest(cols = c(AgeList)) |>
               filter(!is.na(AgeList)) |>
               group_by(location_name, year, AgeList) |>
               mutate(AverageCases = Cases / ((EndAge - StartAge)*100 + 1)) |>
               ungroup() |>
               select(location_name, year, sex_name, Age = AgeList, AverageCases) |>
               group_by(location_name, sex_name, year) |>
               mutate(Weight = AverageCases / sum(AverageCases),
                      Weight = case_when(is.na(Weight) ~ 0,
                                         TRUE ~ Weight),
                      cum_weight = cumsum(Weight)) |>
               summarise(MedianAge = Age[min(which(cum_weight >= 0.5))],
                         Q1 = Age[min(which(cum_weight >= 0.25))],
                         Q3 = Age[min(which(cum_weight >= 0.75))],
                         .groups = 'drop')
          
          GlobalData$data_median <- Data
          
          current_logs <- GlobalData$logs
          new_log <- paste(Sys.time(), "- Estimate median age success")
          GlobalData$logs <- paste(current_logs, new_log, sep = "\n")
     })
     
     ## Plot -------------------------------------------------------------------
     observeEvent(GlobalData$data_median,{
          data_median_age <- GlobalData$data_median
          
          output$outcome_1 <- renderPlot({
               ggplot(data_median_age)+
                    geom_line(aes(x = year, y = MedianAge, color = sex_name))+
                    geom_ribbon(aes(x = year, ymin = Q1, ymax = Q3, fill = sex_name), alpha = 0.5)+
                    facet_wrap(.~location_name)+
                    scale_y_continuous(limits = c(0, NA),
                                       expand = expansion(mult = c(0, 0.3))) +
                    theme_bw()+
                    theme(plot.title.position = "plot",
                          legend.direction = "vertical",
                          legend.position = "bottom")+
                    labs(x = "Year",
                         y = "Median Age of Cases",
                         fill = 'Sex',
                         color = 'Sex')
          })
          
          output$downloadData <- downloadHandler(
               filename = function() {
                    paste("median-age-", Sys.Date(), ".csv", sep="")
               },
               content = function(file) {
                    write.csv(data_median_age, file)
               }
          )
     })
}

# Run the Shiny app
shinyApp(ui, server)
