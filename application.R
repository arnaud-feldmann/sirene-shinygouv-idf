library(shiny)
library(leaflet)
library(RSQLite)
library(here)
library(dplyr)
library(stringr)
library(DT)
library(sp)
library(tidyr)
library(readr)

`%||%` <- function (x, y) if (is.null(x)) y else x

con <- dbConnect(RSQLite::SQLite(),
                 here("retraitement", "sqlite", "db.sqlite"),
                 flags = SQLITE_RO)

tbl_a88_a17 <- 
  read_csv(here("a17_a88t.csv"),
           col_types = cols_only(A17 = col_character(),
                                 A88 = col_character(),
                                 lbl = col_character()))
list_a88_a17 <-
  tbl_a88_a17 %>%
  nest(data = c(A88, lbl)) %>%
  mutate(data = lapply(data,\(tbl) tbl %>% pull(A88) %>% setNames(tbl %>% pull(lbl)))) %>%
  (\(tbl) tbl %>% pull(data) %>% setNames(tbl %>% pull(A17)))

list_tranches <-
  c(
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
tbl_tranches <-
  tibble(tr_label = names(list_tranches),
         tranche = list_tranches)

BOUNDS_IDF <- c(0.04287105, 48.0487500, 4.65712895, 49.6792500)
CENTRE_DEFAUT <-
  c(
    (BOUNDS_IDF[1L] + BOUNDS_IDF[3L]) / 2,
    (BOUNDS_IDF[2L] + BOUNDS_IDF[4L]) / 2
  )
TAILLE_DEFAUT <- 500L

MULTIPLE_ANGLE <- 50000

ui <- navbarPage(
  title = "Etablissements d'Île-de-France",
  windowTitle = "Etablissements d'Île-de-France",
  id = "menu",
  selected = "Carte",
  shiny::tabPanel("Données",
                  fluidRow(column(3L,
                                  selectInput(
                                    "tranches",
                                    "Tranche d'Effectifs",
                                    list_tranches,
                                    selected = unname(list_tranches),
                                    multiple = TRUE,
                                    selectize = TRUE
                                  )),
                           column(3L,
                                  selectInput(
                                    "a88",
                                    "A88",
                                    list_a88_a17,
                                    selected = unname(do.call(c, list_a88_a17)),
                                    multiple = TRUE,
                                    selectize = FALSE
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
        dbGetQuery(con,
                   paste0(
                     "SELECT * FROM stock_etabs_geoloc_idf",
                     " WHERE SUBSTR(activitePrincipaleEtablissement, 1, 2) in (",
                     paste0("'", a88, "'", collapse = ","),
                     ") AND trancheEffectifsEtablissement in (",
                     paste0("'",tranches, "'", collapse = ","),
                     ") AND x_longitude < ", center[1L] + taille / MULTIPLE_ANGLE,
                     " AND x_longitude > ", center[1L] - taille / MULTIPLE_ANGLE,
                     " AND y_latitude < ", center [2L] + taille / MULTIPLE_ANGLE,
                     " AND y_latitude > ", center [2L] - taille / MULTIPLE_ANGLE)) %>%
        mutate(distance_point = spDistsN1(pts = cbind(x_longitude, y_latitude),
                                          pt = center,
                                          longlat = TRUE)) %>%
        filter(distance_point <= taille/1000) %>%
        arrange(distance_point) %>%
        head(10000L) %>%
        mutate(A88 = str_sub(activitePrincipaleEtablissement, 1L, 2L)) %>%
        left_join(tbl_tranches,
                  by = c("trancheEffectifsEtablissement" = "tranche")) %>%
        left_join(tbl_a88_a17,
                  by =  "A88") %>%
        transmute(siret = siret,
                  `Tranche d'effectifs` = trancheEffectifsEtablissement,
                  Secteur = lbl,
                  adresse = str_c(numeroVoieEtablissement, typeVoieEtablissement, libelleVoieEtablissement,
                                  codePostalEtablissement),
                  `Date de création` = dateCreationEtablissement,
                  x = x_longitude,
                  y = y_latitude,
                  distance = distance_point)
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
      addCircles(lng = ~x,
                 lat = ~y,
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
