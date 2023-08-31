library(shiny)
library(leaflet)
library(arrow)
library(here)
library(dplyr)
library(stringr)
library(DT)
library(sp)



`%||%` <- function (x, y) if (is.null(x)) y else x

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

BOUNDS_IDF <- c(0.3570556640625, 47.7965516475594, 4.9713134765625, 49.4270536132596)
TAILLE_DEFAUT <- 500L
# RAYON_TERRE <- 6371009
# 
# magic_a <- 2 * pi * RAYON_TERRE
# magic_b <- 0.5 * pi / 180
# 
# register_scalar_function(
#   "distance_approximative",
#   function(context, lon1, lat1, lon2, lat2) {
#     x <- lat2 - lat1
#     y <- (lon2 - lon1) * cos((lat2 + lat1) * magic_b)
#     magic_a * sqrt(x^2 + y^2)
#   },
#   in_type = schema(lon1 = float64(), lat1 = float64(), lon2 = float64(), lat2 = float64()),
#   out_type = float64(),
#   auto_convert = FALSE
# )

ui <- navbarPage(
  title = "Etablissements d'Île-de-France",
  windowTitle = "Etablissements d'Île-de-France",
  id = "menu",
  selected = "Données",
  shiny::tabPanel("Données",
                  fluidRow(column(4L,
                                  selectInput(
                                    "tranches",
                                    "Tranche d'Effectifs",
                                    tranches,
                                    selected = NULL,
                                    multiple = TRUE,
                                    selectize = TRUE
                                  )),
                           column(4L,
                                  selectInput(
                                    "a88",
                                    "A88",
                                    a88,
                                    selected = NULL,
                                    multiple = TRUE,
                                    selectize = TRUE
                                  ),
                                  column(2L,
                                         actionButton("actualiser_dt", "Go !")))
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
    center = c(
      (BOUNDS_IDF[1L] + BOUNDS_IDF[3L]) / 2,
      (BOUNDS_IDF[2L] + BOUNDS_IDF[4L]) / 2
    ),
    taille = TAILLE_DEFAUT,
    a88 = character(),
    tranches = character()
  )
  observeEvent(input$actualiser_map ,{
    vars$center <- c(input$map_center$lng, input$map_center$lat)
    vars$taille <- input$taille
  })
  observeEvent(input$actualiser_dt ,{
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
