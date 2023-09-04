library(shiny)
library(leaflet)
library(arrow)
library(here)
library(dplyr)
library(stringr)
library(DT)
library(sp)
library(tidyr)

`%||%` <- function (x, y) if (is.null(x)) y else x

list_a88_a17 <-
  arrow::read_csv_arrow(here("a17_a88t.csv"), as_data_frame = FALSE,
                        col_types = schema(A88 = string())) %>%
  collect() %>%
  nest(data = c(A88, lbl)) %>%
  mutate(data = lapply(data,\(tbl) tbl %>% pull(A88) %>% setNames(tbl %>% pull(lbl)))) %>%
  (\(tbl) tbl %>% pull(data) %>% setNames(tbl %>% pull(A17)))

etabs <- read_parquet(here("retraitement", "etabs.parquet"), as_data_frame = FALSE)
list_tranches <-
  list(
    `Etablissement non employeur` = "NN",
    `1 ou 2 salariés` = "01",
    `3 à 5 salariés` = "02",
    `6 à 9 salariés` = "03",
    `10 à 19 salariés` = "11",
    `20 à 49 salariés` = "12",
    `50 à 99 salariés` = "21",
    `100 à 199 salariés` = "22",
    `200 à 249 salariés` = "31",
    `250 à 499 salariés` = "32",
    `500 à 999 salariés` = "41",
    `1 000 à 1 999 salariés` = "42",
    `2 000 à 4 999 salariés` = "51",
    `5 000 à 9 999 salariés` = "52",
    `5 000 à 9 999 salariés` = "53"
  )
  
BOUNDS_IDF <- c(0.3570556640625, 47.7965516475594, 4.9713134765625, 49.4270536132596)
CENTRE_DEFAUT <-
  c(
    (BOUNDS_IDF[1L] + BOUNDS_IDF[3L]) / 2,
    (BOUNDS_IDF[2L] + BOUNDS_IDF[4L]) / 2
  )
TAILLE_DEFAUT <- 500L

ui <- navbarPage(
  title = "Etablissements d'Île-de-France",
  windowTitle = "Etablissements d'Île-de-France",
  id = "menu",
  selected = "Données",
  shiny::tabPanel("Données",
                  fluidRow(column(3L,
                                  selectInput(
                                    "tranches",
                                    "Tranche d'Effectifs",
                                    list_tranches,
                                    selected = NULL,
                                    multiple = TRUE,
                                    selectize = TRUE
                                  )),
                           column(3L,
                                  selectInput(
                                    "a88",
                                    "A88",
                                    list_a88_a17,
                                    selected = NULL,
                                    multiple = TRUE,
                                    selectize = TRUE
                                  )),
                           column(3L,
                                  actionButton("actualiser_dt", "Go !"))
                  ),
                  hr(),
                  textOutput("position"),
                  hr(),
                  dataTableOutput("tbl")
  ),
  shiny::tabPanel("Carte",
                  div(class = "outer",
                      tags$head(
                        includeCSS("styles.css")
                      ),
                      leafletOutput("map", width = "100%", height = "100%"),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    numericInput("taille",
                                                 label = "Taille du cercle (m) :",
                                                 value = TAILLE_DEFAUT,
                                                 min = 0,
                                                 max = 60000L,
                                                 step = 1L),
                                    actionButton("actualiser_map", "Go !")
                      ),
                      
                      tags$div(id="cite",
                               'Source :', tags$em('Sirene (Insee)'))
                  )
                  
  )
)

server <- function(input, output, session) {
  
  vars <- reactiveValues(
    center = CENTRE_DEFAUT,
    taille = TAILLE_DEFAUT,
    a88 = character(),
    tranches = character()
  )
  observeEvent(c(input$actualiser_map, input$actualiser_dt) ,{
    vars$center <- c(input$map_center$lng, input$map_center$lat) %||% CENTRE_DEFAUT
    vars$taille <- input$taille %||% TAILLE_DEFAUT
    vars$a88 <- input$a88
    vars$tranches <- input$tranches
  })
  
  df <- reactive({
    center <- vars$center
    taille <- vars$taille
    a88 <- vars$a88
    tranches <- vars$tranches
    
    isolate(
      
      ret <-
        etabs %>%
        filter(str_sub(activitePrincipaleEtablissement, 1L, 2L) %in% a88 &
                 trancheEffectifsEtablissement %in% tranches) %>%
        mutate(distance_point = (x_longitude-center[1L])^2 + (y_latitude-center[2L])^2) %>%
        arrange(distance_point) %>%
        head(1000L) %>%
        collect() %>%
        mutate(distance_point = spDistsN1(pts = cbind(x_longitude, y_latitude),
                                          pt = center,
                                          longlat = TRUE)) %>%
        filter(distance_point <= taille/1000) %>%
        select(-distance_point)
    )
    
    return(ret)
  })
  
  output$position <- reactive({
    paste(vars$taille,
          "mètres autour du point",
          sprintf("%.3f",vars$center[1L]),
          sprintf("%.3f",vars$center[2L]))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(BOUNDS_IDF[1L], BOUNDS_IDF[2L], BOUNDS_IDF[3L], BOUNDS_IDF[4L])
  })
  
  observe({
    leafletProxy("map",
                 data = df()) %>%
      clearShapes() %>%
      addCircles(lng = ~vars$center[1L],
                 lat = ~vars$center[2L],
                 radius = vars$taille,
                 stroke = FALSE,
                 fillOpacity = 0.2,
                 fillColor = "black") %>%
      addCircles(lng = ~x_longitude,
                 lat = ~y_latitude,
                 radius = 5 ,
                 stroke = FALSE,
                 fillOpacity = 0.8
                 
      )
  })
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(
      df(),
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
