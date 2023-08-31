library(shiny)
library(magrittr)
library(leaflet)
library(DT)

ships <-
  read.csv(
    "https://raw.githubusercontent.com/Appsilon/crossfilter-demo/master/app/ships.csv"
  )

ui <- shinyUI(fluidPage(
  titlePanel("Filter"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 numericInput(
                   "speed_f", label = h5("Ship's Speed"), value = 100
                 )),
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(
        "Leaflet",
        leafletOutput("leafletmap", width = "350px"),
        dataTableOutput("tbl")
      )
    ))
  )
))

server <- function(input, output) {
  in_bounding_box <- function(data, lat, long, bounds, speed) {
    data %>%
      dplyr::filter(
        lat > bounds$south &
          lat < bounds$north &
          long < bounds$east & long > bounds$west & 
          speed > input$speed_f
      )
  }
  
  map_data_react <- reactive({
    
    ships %>% dplyr::filter(speed > input$speed_f)
    
  })
  
  
  output$leafletmap <- renderLeaflet({
    
    ships_data <- map_data_react()  # Add this
    
    ships_data %>% leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
      addCircleMarkers(
        ~ long ,  # Removed `data = data_map()`
        ~ lat,
        popup =  ~ speed,
        radius = 5 ,
        stroke = FALSE,
        fillOpacity = 0.8,
        popupOptions = popupOptions(closeButton = FALSE)
      )
  })
  
  data_map <- reactive({
    if (is.null(input$leafletmap_bounds)) {
      ships
    } else {
      bounds <- input$leafletmap_bounds
      in_bounding_box(ships, lat, long, bounds, speed)
    }
  })
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(
      data_map(),
      extensions = "Scroller",
      style = "bootstrap",
      class = "compact",
      width = "100%",
      options = list(
        deferRender = TRUE,
        scrollY = 300,
        scroller = TRUE,
        dom = 'tp'
      )
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
