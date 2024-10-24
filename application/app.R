
library(shinyBS)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(openxlsx)
library(datamods)
library(lubridate)

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
               )
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
               verbatimTextOutput(outputId = "data_filter_summary")
          )
     ),
     ## KDE -----------------------------
     box(
          title = 'Step 3: Kernel Density Estimation Setting',
          # setting model for Kernel Density Estimation
          width = 12,
          collapsible = T,
          status = 'danger',
          conditionalPanel(
               condition = "output.data_filter_summary",
               fluidRow(
                    column(
                         width = 3,
                         # setting bw for Kernel Density Estimation
                         selectInput(
                              inputId = "model_bw",
                              label = HTML('Bandwidth <i id="info-bw" class="fa fa-info-circle"></i>'),
                              choices = c("nrd0", "nrd", "ucv", "bcv", 'SJ'),
                              selected = "nrd0"
                         ),
                         bsTooltip(id = "info-bw",
                                   title = "the smoothing bandwidth to be used. The default is ‘nrd0’.",
                                   placement = "right", trigger = "hover")
                    ),
                    column(
                         width = 3,
                         # setting adjust for Kernel Density Estimation
                         numericInput(
                              inputId = "model_adjust",
                              label = HTML('Adjust <i id="info-adjust" class="fa fa-info-circle"></i>'),
                              value = 1
                         ),
                         bsTooltip(id = "info-adjust",
                                   title = "the bandwidth used is actually adjust*bw. This makes it easy to specify values like ‘half the default’ bandwidth.",
                                   placement = "right", trigger = "hover")
                    ),
                    column(
                         width = 3,
                         # setting kernel for Kernel Density Estimation
                         selectInput(
                              inputId = "model_kernel",
                              label = HTML('Kernel <i id="info-kernel" class="fa fa-info-circle"></i>'),
                              choices = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
                              selected = "gaussian"
                         ),
                         bsTooltip(id = "info-kernel",
                                   title = 'a character string giving the smoothing kernel to be used.  <br> This must partially match one of "gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine" or "optcosine", with default "gaussian", and may be abbreviated to a unique prefix (single letter). "cosine" is smoother than "optcosine", which is the usual ‘cosine’ kernel in the literature and almost MSE-efficient. However, "cosine" is the version used by S.',
                                   placement = "right", trigger = "hover")
                    ),
                    column(
                         width = 3,
                         # setting n for Kernel Density Estimation
                         numericInput(
                              inputId = "model_n",
                              label = HTML('n <i id="info-n" class="fa fa-info-circle"></i>'),
                              value = 512
                         ),
                         bsTooltip(id = "info-n",
                                   title = "the number of equally spaced points at which the density is to be estimated. The default is 512.",
                                   placement = "right", trigger = "hover")
                    )
               ),
               fluidRow(
                    column(
                         width = 3,
                         # setting from for Kernel Density Estimation
                         numericInput(
                              inputId = "model_from",
                              label = HTML('From <i id="info-from" class="fa fa-info-circle"></i>'),
                              value = 0
                         ),
                         bsTooltip(id = "info-from",
                                   title = "the minimum of the range of x values for which density is to be estimated. The default is the minimum of the data values.",
                                   placement = "right", trigger = "hover")
                    ),
                    column(
                         width = 3,
                         # setting to for Kernel Density Estimation
                         numericInput(
                              inputId = "model_to",
                              label = HTML('To <i id="info-to" class="fa fa-info-circle"></i>'),
                              value = 100
                         ),
                         bsTooltip(id = "info-to",
                                   title = "the maximum of the range of x values for which density is to be estimated. The default is the maximum of the data values.",
                                   placement = "right", trigger = "hover")
                    ),
                    column(
                         width = 3,
                         # start for Kernel Density Estimation
                         actionButton(
                              inputId = "start_kde",
                              label = "Start KDE",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 25px;"
                         )
                    )
               )
          )
     ),
     box(
          title = 'Step 4: Outcome',
          width = 12,
          collapsible = T,
          status = 'danger'
     ),
     # add footer
     column(
          width = 12,
          tags$footer(
               tags$p(
                    "Author: Kangguo Li",
                    tags$br(),
                    tags$em("Version 2.0.0"),
                    tags$br(),
                    tags$em("Last updated: 2024-03-12"),
                    tags$br(),
                    tags$em("Host: shinyapps.io"),
                    tags$br(),
                    tags$a(
                         href = "https://github.com/xmusphlkg/code_PHSM",
                         icon("github"),
                         title = "Source Code",
                         style = "margin: 10px"
                    ),
                    tags$a(
                         href = "https://github.com/xmusphlkg/code_PHSM",
                         icon("file-pdf"),
                         title = "DOI",
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
          data_age = NULL,
          data_median = NULL
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

          output$filter_ui <- renderUI({
               tags$div(
                    column(
                         6,
                         selectInput(
                              inputId = "data_location_name",
                              label = "Location Name",
                              choices = unique(Data$location_name),
                              selected = unique(Data$location_name),
                              multiple = T
                         )
                    ),
                    column(
                         6,
                         selectInput(
                              inputId = "data_sex_name",
                              label = "Sex Name",
                              choices = unique(Data$sex_name),
                              selected = unique(Data$sex_name),
                              multiple = T
                         )
                    ),
                    column(
                         6,
                         selectInput(
                              inputId = "data_age_name",
                              label = "Age Name",
                              choices = sort(unique(Data$age_name)),
                              selected = sort(unique(Data$age_name)),
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
                         10,
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
          data_filter <- Data  |> 
               filter(location_name %in% input$data_location_name &
                           sex_name %in% input$data_sex_name &
                           age_name %in% input$data_age_name &
                           measure_name == input$data_measure_name &
                           year >= input$data_year_range[1] &
                           year <= input$data_year_range[2]) |> 
               mutate(Age = str_replace_all(age_name, "<", "0-"),
                      # replace "+ years" with "-100 years"
                      Age = str_replace_all(Age, "\\+ years", "-100 years"),
                      Age = str_replace_all(Age, " years", ""),
                      StartAge = as.numeric(sub("-.*", "", Age)),
                      EndAge = as.numeric(sub(".*-", "", Age)),
                      MiddleAge = (StartAge + EndAge) / 2,
                      StartAge = if_else(StartAge == 0, 0, StartAge - 0),
                      EndAge = if_else(EndAge == 100, 100, EndAge + 0.99))
          GlobalData$data_filter <- data_filter
          
          output$data_filter_summary <- renderPrint({
               str(data_filter)
          })
     })
     
     ## KDE ---------------------------------------------------------------------

     # observeEvent(GlobalData$split_Data, {
     #      Data <- GlobalData$split_Data
     #      # detect input$split_by is not null
     #      if (is.null(input$split_by)) {
     #           output$split_info <- renderPrint({
     #                "Not required to split data"
     #           })
     #      } else {
     #           output$split_info <- renderPrint({
     #                paste("Split by", input$split_by, "with value", input$split_type)
     #           })
     #      }
     #      output$split_data <- renderPrint({
     #           str(Data)
     #      })
     #      # auto detect frequency
     #      # browser()
     #      freq <- detect_frequency(Data$date)
     #      if (freq == 365.25) {
     #           Data <- Data |> 
     #                select(date, value) |>
     #                complete(date = seq.Date(min(date), max(date), by = "day"),
     #                         fill = list(value = 0))
     #           start_date <- c(year(min(Data$date)), yday(min(Data$date)))
     #           end_date <- c(year(max(Data$date)), yday(max(Data$date)))
     #      } else if (freq == 12) {
     #           Data <- Data |> 
     #                select(date, value) |>
     #                complete(date = seq.Date(min(date), max(date), by = "month"),
     #                         fill = list(value = 0))
     #           start_date <- c(year(min(Data$date)), month(min(Data$date)))
     #           end_date <- c(year(max(Data$date)), month(max(Data$date)))
     #      } else {
     #           Data <- Data |> 
     #                select(date, value) |>
     #                complete(date = seq.Date(min(date), max(date), by = "day"),
     #                         fill = list(value = 0))
     #           start_date <- year(min(Data$date))
     #           end_date <- year(max(Data$date))
     #      }
     #      
     #      tryCatch({
     #           GlobalData$split_Data_ts <- ts(Data$value, start = start_date, end = end_date, frequency = freq)
     #           output$split_data_ts <- renderPrint({
     #                GlobalData$split_Data_ts
     #           })
     #           showNotification(
     #                ui = "Data has been converted to time-series",
     #                type = "message",
     #                duration = 5
     #           )
     #      }, error = function(e) {
     #           output$split_data_ts <- renderPrint({
     #                paste("Error:", e)
     #           })
     #           showNotification(
     #                ui = "Data cannot be converted to time-series",
     #                type = "error",
     #                duration = 60
     #           )
     #      })
     #      
     #      # update date range
     #      train_period <- round(nrow(Data) * 0.7)
     #      test_period <- nrow(Data) - train_period
     #      train_date <- Data$date[1:train_period]
     #      test_date <- Data$date[(train_period + 1):nrow(Data)]
     #      updateDateRangeInput(
     #           session = session,
     #           inputId = "train_date",
     #           start = min(train_date),
     #           end = max(train_date)
     #      )
     #      updateDateRangeInput(
     #           session = session,
     #           inputId = "test_date",
     #           start = min(test_date),
     #           end = max(test_date)
     #      )
     # })
     # 
     # observeEvent(input$train_model, {
     #      Data <- GlobalData$split_Data
     #      DataTS <- GlobalData$split_Data_ts
     #      train_date <- as.Date(input$train_date)
     #      test_date <- as.Date(input$test_date)
     #      model_type <- input$model_type
     #      
     #      # check date range is legal
     #      if (test_date[1] < train_date[2]) {
     #           showNotification(
     #                ui = "Test date should be later than train date",
     #                type = "error",
     #                duration = 60
     #           )
     #      } else {
     #           train_id <- which(Data$date >= train_date[1] & Data$date <= train_date[2])
     #           test_id <- which(Data$date >= test_date[1] & Data$date <= test_date[2])
     #           all_id <- which(Data$date >= train_date[1] & Data$date <= test_date[2])
     #           train_ts <- DataTS[train_id]
     #           test_ts <- DataTS[test_id]
     #           all_ts <- DataTS[all_id]
     #           
     #           outcome <- auto_select_function(train_ts, test_ts, all_ts, 0.1, model_type)
     #           
     #           output$model_info <- renderPrint({
     #                paste("Model type:", paste(model_type, collapse = ", "))
     #           })
     #           
     #           DataIndex <- get_norm_index(outcome[['goodness']])
     #           output$model_data <- renderDT(DataIndex,
     #                                         options = list(
     #                                              pageLength = 10,
     #                                              autoWidth = TRUE,
     #                                              ordering = TRUE,
     #                                              scrollX = TRUE,
     #                                              scrollY = TRUE,
     #                                              fixedColumns = TRUE,
     #                                              searching = TRUE,
     #                                              info = TRUE,
     #                                              dom = "Bfrtip",
     #                                              buttons = list(
     #                                                   "copy",
     #                                                   "csv",
     #                                                   "excel",
     #                                                   "pdf",
     #                                                   "print"
     #                                              )
     #                                         ),
     #                                         rownames = FALSE,
     #                                         class = "display",
     #                                         extensions = c("Buttons"))
     #           
     #           output$model_summary <- renderPrint({
     #                DataIndex[DataIndex$Best == 1, 1:4]
     #           })
     #           
     #           GlobalData$optimal_model <- as.character(DataIndex[DataIndex$Best == 1, 'Method'])
     #           
     #           # update forecast model
     #           updateSelectInput(
     #                session = session,
     #                inputId = "set_model",
     #                selected = GlobalData$optimal_model
     #           )
     #           updateDateRangeInput(
     #                session = session,
     #                inputId = "forecast_train",
     #                start = min(train_date),
     #                end = max(test_date)
     #           )
     #      }
     # })
     # 
     # observeEvent(input$forecast_model, {
     #      Data <- GlobalData$split_Data
     #      DataTS <- GlobalData$split_Data_ts
     #      train_date <- as.Date(input$forecast_train)
     #      forecast_period <- input$forecast_period
     #      model_type <- input$set_model
     #      
     #      train_id <- which(Data$date >= train_date[1] & Data$date <= train_date[2])
     #      train_ts <- DataTS[train_id]
     #      freq <- frequency(DataTS)
     #      if (freq == 365.25) {
     #           start_date <- c(year(train_date[1]), yday(train_date[1]))
     #           end_date <- c(year(train_date[2]), yday(train_date[2]))
     #      } else if (freq == 12) {
     #           start_date <- c(year(train_date[1]), month(train_date[1]))
     #           end_date <- c(year(train_date[2]), month(train_date[2]))
     #      } else {
     #           start_date <- year(train_date[1])
     #           end_date <- year(train_date[2])
     #      }
     #      train_ts <- ts(train_ts, start = start_date, end = end_date, frequency = freq)
     #      outcome <- auto_forecast_function(train_ts, forecast_period, 0.1, model_type)
     #      
     #      output$forecast_info <- renderPrint({
     #           summary(outcome$mod)
     #      })
     #      GlobalData$forecast_data <- outcome$outcome_plot_2
     #      
     #      output$forecast_data <- renderDT(GlobalData$forecast_data,
     #                                       options = list(
     #                                            pageLength = 5,
     #                                            autoWidth = TRUE,
     #                                            ordering = TRUE,
     #                                            scrollX = TRUE,
     #                                            scrollY = TRUE,
     #                                            fixedColumns = TRUE,
     #                                            searching = TRUE,
     #                                            info = TRUE,
     #                                            dom = "Bfrtip",
     #                                            buttons = list(
     #                                                 "copy",
     #                                                 "csv",
     #                                                 "excel",
     #                                                 "pdf",
     #                                                 "print"
     #                                            )
     #                                       ),
     #                                       rownames = FALSE,
     #                                       class = "display",
     #                                       extensions = c("Buttons"))
     #      
     #      output$forecast_plot <- renderPlot({
     #           ggplot(GlobalData$split_Data) +
     #                geom_line(
     #                     mapping = aes(x = date,
     #                                   y = value,
     #                                   colour = "Observed"),
     #                     linewidth = 0.7
     #                ) +
     #                geom_line(
     #                     mapping = aes(x = date,
     #                                   y = mean,
     #                                   colour = "Forecasted"),
     #                     linewidth = 0.7,
     #                     data = outcome$outcome_plot_2
     #                ) +
     #                scale_x_date(
     #                     expand = expansion(add = c(0, 0)),
     #                     date_labels = "%Y"
     #                ) +
     #                scale_color_manual(values = c(
     #                     Forecasted = "#E64B35FF",
     #                     Observed = "#00A087FF"
     #                )) +
     #                theme_classic()+
     #                theme(legend.position = "bottom") +
     #                labs(
     #                     x = 'Date',
     #                     y = 'Value',
     #                     color = ""
     #                )
     #      })
     # })
}

# Run the Shiny app
shinyApp(ui, server)
