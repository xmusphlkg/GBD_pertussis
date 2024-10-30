
library(shinydashboard)
library(sf)
library(leaflet)
library(plotly)
library(shinyalert)

# read data
# data_country_total <- read.csv('./country.csv')

# prepare map data for leaflet
load('.RData')

# remove iso_a3 is ATA
# data_map <- data_map[data_map$iso_a3 != 'ATA',]
# 
# country_centers <- st_centroid(st_as_sf(data_map))
# country_centers <- data.frame(
#      ISO3 = data_map$iso_a3,
#      lat = st_coordinates(country_centers)[,2],
#      lng = st_coordinates(country_centers)[,1]
# )
# save(country_centers, data_map, file = 'map.RData')

# data_map_iso <- read.csv('./iso_code.csv')

# data_country_total <- merge(data_country_total, data_map_iso, by.x = "location_name", by.y = "location_name", all.x = TRUE)

# var_country <- unique(data_country_total$location_name)
# var_year <- sort(unique(data_country_total$year))

ui <- dashboardPage(
     dashboardHeader(title = "GBD 2021 CFR"),
     dashboardSidebar(
          # select year
          selectInput("year", "Year", choices = var_year, selected = 2019),
          # select index
          selectInput("index", "Index", choices = c("CFR" = "CFR",
                                                    "Incidence rate" = "Incidence",
                                                    "Mortality rate" = "Deaths"),
                      selected = "CFR"),
          # select country
          selectInput("country", "Country", choices = var_country, selected = "United States of America"),
          # initialize the map
          actionButton("reset", "Start Over"),
          # hint
          tags$hr(),
          tags$b("Hint: If get error, please click the 'Start Over' button, again", style = "color: #dd4b39; padding: 20px;")
     ),
     dashboardBody(
          # remove the border of the box
          tags$style(type = "text/css", ".content {padding:0px;}"),
          # set height of the map is 100%
          tags$style(type = "text/css", "#map {height: calc(100vh - 50px) !important;}"),
          leafletOutput("map"),
          absolutePanel(
               id = 'national_right',
               top = 90,
               right = 15,
               left = 'auto',
               buttom = 15,
               width = 400,
               height = 'auto',
               fixed = T,
               draggable = T,
               style = "background-color: rgba(128, 128, 128, 0.5); padding-left: 5px; padding-right: 5px;padding-top: 5px; color: white;",
               tags$h3("Country Details", style = "color: white;"),
               conditionalPanel(
                    condition = "Shiny.shinyapp.$inputValues.map_shape_click != null",
                    tags$div(
                         tags$hr(),
                         tags$p("Incidence", style = "color: white"),
                         plotlyOutput(
                              outputId = 'incidence',
                              width = '100%',
                              height = '120px'
                         ),
                         tags$p("Morality", style = "color: white"),
                         plotlyOutput(
                              outputId = 'mortality',
                              width = '100%',
                              height = '120px'
                         ),
                         tags$p("CFR", style = "color: white"),
                         plotlyOutput(
                              outputId = "CFR",
                              width = '100%',
                              height = '120px'
                         ),
                         downloadButton("download", "Download the data")
                    )
               ),
               tags$hr(),
               tags$b("Hint: Click on the map to see the details", style = "color: #dd4b39; merge: 20px;")
          )
     )
)

server <- function(input, output, session) {
     # update the plot
     observeEvent(input$map_shape_click, {
          # user input
          id <- input$map_shape_click$id
          location_name <- data_map_iso[data_map_iso$ISO3 == id, 'location_name']
          
          # if the location_name is null, show the hint
          if (length(location_name) == 0) {
               shinyalert("Error", "No data for this country", type = "error")
          } else {
               # update select
               updateSelectInput(session, "country", selected = location_name)
               
               # prepare map data for leaflet
               data <- data_country_total[data_country_total$location_name == location_name,]
               
               # arrange the data
               data <- data[order(data$year),]
               
               # browser()
               
               # update plot, using plotly to create the curve
               output$incidence <- renderPlotly({
                    plot_ly(data, mode = "lines", x = ~year) |> 
                         add_trace(
                              y = ~Incidence,
                              name = "Incidence",
                              type = 'scatter',
                              line = list(color = 'red'),
                              hoverinfo = "text", 
                              text = ~paste("Year: ", year, "<br>Incidence: ", round(Incidence, 4))) |>
                         layout(
                              yaxis = list(
                                   showticklabels = T,
                                   title = "",
                                   color = 'white'
                              ),
                              xaxis = list(
                                   showticklabels = T,
                                   title = "",
                                   type = 'int',
                                   color = 'white'
                              ),
                              showlegend = F,
                              plot_bgcolor = 'rgba(0, 0, 0, 0)',
                              paper_bgcolor = 'rgba(0, 0, 0, 0)',
                              margin = list(
                                   b = 0,
                                   t = 0,
                                   l = 0,
                                   pad = 0
                              )
                         )
                    
               })
               
               output$mortality <- renderPlotly({
                    plot_ly(data, mode = "lines", x = ~year) |> 
                         add_trace(
                              y = ~Deaths,
                              name = "Mortality",
                              type = 'scatter',
                              line = list(color = 'blue'),
                              hoverinfo = "text", 
                              text = ~paste("Year: ", year, "<br>Mortality: ", round(Deaths, 4))) |>
                         layout(
                              yaxis = list(
                                   showticklabels = T,
                                   title = "",
                                   color = 'white'
                              ),
                              xaxis = list(
                                   showticklabels = T,
                                   title = "",
                                   type = 'int',
                                   color = 'white'
                              ),
                              showlegend = F,
                              plot_bgcolor = 'rgba(0, 0, 0, 0)',
                              paper_bgcolor = 'rgba(0, 0, 0, 0)',
                              margin = list(
                                   b = 0,
                                   t = 0,
                                   l = 0,
                                   pad = 0
                              )
                         )
               })
               
               output$CFR <- renderPlotly({
                    plot_ly(data, mode = "lines", x = ~year) |> 
                         add_trace(
                              y = ~CFR,
                              name = "CFR",
                              type = 'scatter',
                              line = list(color = 'green'),
                              hoverinfo = "text", 
                              text = ~paste("Year: ", year, "<br>CFR: ", round(CFR, 4))) |>
                         layout(
                              yaxis = list(
                                   showticklabels = T,
                                   title = "",
                                   color = 'white'
                              ),
                              xaxis = list(
                                   showticklabels = T,
                                   title = "",
                                   type = 'int',
                                   color = 'white'
                              ),
                              showlegend = F,
                              plot_bgcolor = 'rgba(0, 0, 0, 0)',
                              paper_bgcolor = 'rgba(0, 0, 0, 0)',
                              margin = list(
                                   b = 0,
                                   t = 0,
                                   l = 0,
                                   pad = 0
                              )
                         )
               })
               
               # download data
               output$download <- downloadHandler(
                    filename = function() {
                         paste(location_name, ".csv", sep = "")
                    },
                    content = function(file) {
                         write.csv(data, file, row.names = F)
                    }
               )
          }
     })
     
     # move click country
     observeEvent(input$country, {
          location_name <- input$country
          iso_a3 <- data_map_iso[data_map_iso$location_name == location_name, 'ISO3']
          
          if (iso_a3 %in% country_centers$ISO3) {
               center <- country_centers[country_centers$ISO3 == iso_a3, ]
               lat <- center$lat
               lng <- center$lng
               
               leafletProxy("map") |> 
                    setView(lng = lng, lat = lat, zoom = 4)
          } else {
               leafletProxy("map")|>
                    setView(zoom = 2, lat = 30, lng = 0)
          }
     })
     
     # reset the map
     observeEvent(input$reset, {
          output$map <- renderLeaflet({
               # test
               # index <- 'CFR'
               # year <- 2019
               
               # user input
               index <- input$index
               year <- input$year
               
               # filter data by year
               data <- data_country_total[data_country_total$year == year,]
               
               # add new column
               data['value'] <- data[index]
               data_fill <- merge(data_map, data, by.x = "iso_a3", by.y = "ISO3", all.x = TRUE)
               
               # remove NA
               data_fill <- data_fill[!is.na(data_fill$value),]
               
               # create color palette
               breaks <- pretty(data$value, n = 7)
               limits <- range(breaks, na.rm = TRUE)
               pal <- colorBin("YlOrRd", domain = data$value, bins = breaks, na.color = "transparent")
               
               # create label
               overview_labels <- sprintf(
                    "<strong>%s (%s)</strong><br/>Incidence:%s<br/>Mortality:%s<br/>CFR:%s",
                    data_fill$location_name,
                    data_fill$iso_a3,
                    round(data_fill$Incidence, 4),
                    round(data_fill$Deaths, 4),
                    round(data_fill$CFR, 4)
               ) |>
                    lapply(htmltools::HTML)
               
               # Creating the Leaflet map
               map <- leaflet(data = data_fill)|>
                    # add tile
                    addProviderTiles("CartoDB.Positron")
               
               # add polygons
               map <- map |>
                    addPolygons(
                         data = data_fill,
                         fillColor = ~pal(value),
                         layerId = ~iso_a3,
                         weight = 2,
                         opacity = 1.0,
                         fillOpacity = 0.85,
                         color = "grey",
                         label = overview_labels,
                         labelOptions = labelOptions(
                              style = list("font-weight" = "normal", "padding" = "3px 8px"),
                              direction = "auto",
                              clickable = TRUE
                         ),
                         group = "CFR"
                    ) |> 
                    # add legend
                    addLegend(
                         pal = pal,
                         values = ~value,
                         opacity = 0.7,
                         title = paste(index, " ", year),
                         position = "bottomleft"
                    )
               
               map
          })
     })
}

shinyApp(ui, server)