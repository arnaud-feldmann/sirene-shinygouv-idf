library(shiny)
library(leaflet)
library(arrow)
library(here)
library(dplyr)
library(stringr)
library(DT)

etabs <<- read_parquet(here("retraitement", "etabs.parquet"), as_data_frame = FALSE)
tranches <<-
  etabs %>%
  distinct(trancheEffectifsEtablissement) %>%
  filter(!is.na(trancheEffectifsEtablissement)) %>%
  arrange(trancheEffectifsEtablissement) %>%
  collect()
a88 <<-
  etabs %>%
  mutate(A88 = str_sub(activitePrincipaleEtablissement, 1L, 2L)) %>%
  distinct(A88) %>%
  filter(!is.na(A88)) %>%
  arrange(A88) %>%
  collect()

bounds_idf <- c(0.3570556640625, 47.7965516475594, 4.9713134765625, 49.4270536132596)

ui <- navbarPage(
  title = "Etablissements d'Île-de-France",
  windowTitle = "Etablissements d'Île-de-France",
  id = "menu",
  selected = "Données",
  shiny::tabPanel("Données",
                  fluidRow(column(6L,
                                  selectInput(
                                    "tranches",
                                    "Tranche d'Effectifs",
                                    tranches,
                                    selected = NULL,
                                    multiple = TRUE,
                                    selectize = TRUE
                                  )),
                           column(6L,
                                  selectInput(
                                    "a88",
                                    "A88",
                                    a88,
                                    selected = NULL,
                                    multiple = TRUE,
                                    selectize = TRUE
                                  ))
                  ),
                  hr(),
                  dataTableOutput("tbl")
  ),
  shiny::tabPanel("Carte",
                  div(class = "outer",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("styles.css")
                      ),
                      leafletOutput("map", width = "100%", height = "100%")
                  )
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    center <- input$map_center
    if (is.null(center)) {
      center <- list(
        lng = (bounds_idf[1L] + bounds_idf[3L]) / 2L,
        lat = (bounds_idf[2L] + bounds_idf[4L]) / 2L
      )
    }
    
    etabs %>%
      filter(str_sub(activitePrincipaleEtablissement, 1L, 2L) %in% input$a88 &
               trancheEffectifsEtablissement %in% input$tranches) %>%
      mutate(distance_point = (center$lng - x_longitude)^2 + (center$lag - y_latitude)^2) %>%
      arrange(desc(distance_point)) %>%
      head(1000L) %>%
      collect()
    
  })
  
  output$map <- renderLeaflet({
    filteredData %>%
      leaflet() %>%
      addTiles() %>%
      fitBounds(bounds_idf[1L], bounds_idf[2L], bounds_idf[3L], bounds_idf[4L])
  })
  
  observe({
    
    leafletProxy("map",
                 data = filteredData()) %>%
      clearShapes() %>%
      addCircleMarkers(lng = ~x_longitude,
                       lat = ~y_latitude,
                       radius = 5 ,
                       stroke = FALSE,
                       fillOpacity = 0.8
                       
      )
  })
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(
      filteredData(),
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

shinyApp(ui, server)
