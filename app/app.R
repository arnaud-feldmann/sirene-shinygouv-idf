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
library(shinyWidgets)
library(htmlwidgets)
#remotes::install_github("spyrales/shinygouv")
library(shinygouv)
library(promises)
library(future)
plan(multisession, workers = 4)

`%||%` <- function (x, y) if (is.null(x)) y else x

tbl_a88_a17 <- 
  read_csv(here("a17_a88t.csv"),
           col_types = cols_only(A17 = col_character(),
                                 A88 = col_character(),
                                 A88_lbl = col_character()))
list_a88_a17 <-
  tbl_a88_a17 %>%
  nest(data = c(A88, A88_lbl)) %>%
  mutate(data = lapply(data,\(tbl) tbl %>% pull(A88) %>% setNames(tbl %>% pull(A88_lbl)))) %>%
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
  tibble(tranche_lbl = names(list_tranches),
         tranche = list_tranches)

BOUNDS_IDF <- c(0.04287105, 48.0487500, 4.65712895, 49.6792500)
CENTRE_DEFAUT <-
  c(
    (BOUNDS_IDF[1L] + BOUNDS_IDF[3L]) / 2,
    (BOUNDS_IDF[2L] + BOUNDS_IDF[4L]) / 2
  )
TAILLE_DEFAUT <- 500L
TAILLE_MAX <- 10000L
TRANCHES_SEL_DEFAUT <- unname(list_tranches)
A88_SEL_DEFAUT <- unname(do.call(c, list_a88_a17))
MULTIPLE_ANGLE_X <- 70000
MULTIPLE_ANGLE_Y <- 110000
# Ces multiples sélectionnent un carré pour la TAILLE_MAX 10000 en Île-de-France,
# de sorte à ce qu'on ne manque aucun établissement en faisant le rond à
# l'intérieur après la requête (plus simple) de carré.

get_query <- function(a88 = A88_SEL_DEFAUT, tranches = TRANCHES_SEL_DEFAUT,
                      center = CENTRE_DEFAUT, taille = TAILLE_DEFAUT) {
  
  con <- dbConnect(RSQLite::SQLite(),
                   here("sqlite", "db.sqlite"),
                   flags = SQLITE_RO,
                   extended_types = TRUE)
  
  X_VOISINAGE <- taille / MULTIPLE_ANGLE_X
  Y_VOISINAGE <- taille / MULTIPLE_ANGLE_Y
  
  ret <-
    dbGetQuery(con,
             paste0(
               "SELECT etab.*, ent.denominationUniteLegale, ent.trancheEffectifsUniteLegale, ",
               "ent.categorieJuridiqueUniteLegale, ent.economieSocialeSolidaireUniteLegale, ",
               "ent.nicSiegeUniteLegale, ent.denominationUsuelleUniteLegale ",
               "FROM stock_etabs_geoloc_idf as etab, stock_ent_idf as ent ",
               "WHERE SUBSTR(activitePrincipaleEtablissement, 1, 2) in (",
               paste0("'", a88, "'", collapse = ","),
               ") AND trancheEffectifsEtablissement in (",
               paste0("'",tranches, "'", collapse = ","),
               ") AND x_longitude BETWEEN ", center[1L] - X_VOISINAGE, " AND ", center[1L] + X_VOISINAGE,
               " AND y_latitude BETWEEN ", center[2L] - Y_VOISINAGE, " AND ", center[2L] + Y_VOISINAGE,
               " AND etab.siren = ent.siren")) %>%
    mutate(distance_point = spDistsN1(pts = cbind(x_longitude, y_latitude),
                                      pt = center,
                                      longlat = TRUE)) %>%
    as_tibble() %>%
    filter(distance_point <= taille/1000) %>%
    arrange(distance_point) %>%
    head(10000L) %>%
    mutate(A88 = str_sub(activitePrincipaleEtablissement, 1L, 2L),
           siret = str_c(siren, nic),
           siret_siege = str_c(siren, nicSiegeUniteLegale)) %>%
    left_join(tbl_tranches %>% rename(`Tranche d'effectifs (établissement)` = tranche_lbl),
              by = c("trancheEffectifsEtablissement" = "tranche")) %>%
    left_join(tbl_tranches %>% rename(`Tranche d'effectifs (entreprise)` = tranche_lbl),
              by = c("trancheEffectifsUniteLegale" = "tranche")) %>%
    left_join(tbl_a88_a17,
              by =  "A88") %>%
    transmute(SIRET = siret,
              `SIRET (siège)` = siret_siege,
              `Nom d'établissement` = enseigneEtablissement,
              `Nom d'entreprise` = denominationUniteLegale,
              Secteur = A88_lbl,
              `Tranche d'effectifs (établissement)` = `Tranche d'effectifs (établissement)`,
              `Tranche d'effectifs (entreprise)` = `Tranche d'effectifs (entreprise)`,
              Adresse = str_c(numeroVoieEtablissement, typeVoieEtablissement, libelleVoieEtablissement,
                              codePostalEtablissement, libelleCommuneEtablissement, sep = " "),
              CJ = categorieJuridiqueUniteLegale,
              `Economie Sociale et Solidaire` = economieSocialeSolidaireUniteLegale,
              `Date de création` = dateCreationEtablissement,
              `Dénomination usuelle (établissement)` = denominationUsuelleEtablissement,
              `Dénomination usuelle (entreprise)` = denominationUsuelleUniteLegale,
              Longitude = x_longitude,
              Latitude = y_latitude,
              `Distance au point` = distance_point
    )
  dbDisconnect(con)
  ret
}

ui <- navbarPage_dsfr(
  title = "Etablissements d'Île-de-France",
  header = header_dsfr(intitule = "Drieets",
                       nom_site_service = "Etablissements d'Île-de-France",
                       baseline = "Service Etudes-Statistiques-Evaluation"),
  id = "menu",
  navbarPanel_dsfr("Carte",
                   div(class = "outer",
                       tags$head(
                         includeCSS("styles.css"),
                         tags$style("strong, b { font-weight: bold; }"),
                         tags$script("
                         cercle_centre = null;
                         Shiny.addCustomMessageHandler('taille', function(taille) {
                          if (cercle_centre !== null) {
                            cercle_centre.setRadius(taille);
                          }
                         });"
                         ),
                         includeScript("centercross.js")
                       ),
                       leafletOutput("map", width = "100%", height = "100%"),
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = FALSE, top = 190, left = "auto", right = 20, bottom = "auto",
                                     width = 330, height = "auto",
                                     numericInput_dsfr("taille",
                                                       label = HTML("<b>Taille du cercle (m)</b>"),
                                                       value = TAILLE_DEFAUT,
                                                       min = 0L,
                                                       max = TAILLE_MAX,
                                                       step = 1L),
                                     div(
                                       virtualSelectInput(
                                         "tranches",
                                         HTML("<b>Tranche d'Effectifs</b>"),
                                         list_tranches,
                                         selected = TRANCHES_SEL_DEFAUT,
                                         multiple = TRUE,
                                         selectAllText = "Tout sélectionner",
                                         allOptionsSelectedText = "Tout",
                                         placeholder = "Sélection vide",
                                         position = "top right"
                                       ),
                                       style = "padding-top: 12px; padding-bottom: 3px"
                                     ),
                                     div(
                                       virtualSelectInput(
                                         "a88",
                                         HTML("<b>A88</b>"),
                                         list_a88_a17,
                                         selected = A88_SEL_DEFAUT,
                                         multiple = TRUE,
                                         selectAllText = "Tout sélectionner",
                                         allOptionsSelectedText = "Tout",
                                         placeholder = "Sélection vide",
                                         position = "right"
                                       ),
                                       style = "padding-top: 3px; padding-bottom: 36px"
                                     ),
                                     actionButton_dsfr("actualiser_map", "Go !"),
                                     div(textOutput("position"), style = "padding-top: 20px")
                       ),
                       
                       div(id="cite",
                           'Source :', tags$em('Sirene (Insee)'))
                   )
                   
  ),
  navbarPanel_dsfr("Données",
                   fluidRow_dsfr(
                     column_dsfr(3L,
                                 div(
                                   HTML("<label id='filtre-label' for='filtre'><b>Filtrer</b></label>"),
                                   div(id = "filtre"),
                                   style = "padding: 10px")
                     ),
                     column_dsfr(6L),
                     column_dsfr(3L,div(
                       HTML("<label id='boutons-label' for='boutons'><b>Télécharger</b></label>"),
                       div(id = "boutons"),
                       style = "padding: 10px")
                     )
                   ),
                   DTOutput("tbl")
  )
)

server <- function(input, output, session) {
  
  center <- reactive(c(input$map_center$lng, input$map_center$lat) %||% CENTRE_DEFAUT)
  taille <- reactive(input$taille %||% TAILLE_DEFAUT |> min(TAILLE_MAX + 1L))
  a88 <- reactive(input$a88 %||% A88_SEL_DEFAUT)
  tranches <- reactive(input$tranches %||% TRANCHES_SEL_DEFAUT)
  
  df <- reactiveVal(value = get_query())
  
  observeEvent(input$actualiser_map,
               {
                 a88 <- a88()
                 tranches <- tranches()
                 center <- center()
                 taille <- taille()
                 future_promise({
                   get_query(a88, tranches, center, taille)
                 }) %...>%
                   df %...>%
                   (function(x) {
                     if (NROW(df()) >= 10000L) {
                       shiny::showModal(
                         shiny::modalDialog(title = "Trop de résultats !",
                                            "La recherche a plus de 10000 résultats, seuls les 10000 les plus proches ont été retenus",
                                            easyClose = TRUE,
                                            footer = NULL),
                         session)
                     }
                   })
               })
  
  output$position <- reactive({
    sprintf("%d m autour du point (%.3f, %.3f)", taille(), center()[1L], center()[2L])
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8,
                                     zoomControl= FALSE)) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setMaxBounds(BOUNDS_IDF[1L], BOUNDS_IDF[2L], BOUNDS_IDF[3L], BOUNDS_IDF[4L]) %>%
      fitBounds(BOUNDS_IDF[1L], BOUNDS_IDF[2L], BOUNDS_IDF[3L], BOUNDS_IDF[4L]) %>%
      onRender("
            function(el,x) {
                let mymap = this;
                var control = L.centerCross();
                mymap.addLayer(control);
                cercle_centre = L.circle(mymap.getCenter(), {radius: 500, stroke: false, fillColor: 'black', fillOpacity: 0.2}).addTo(mymap);
                mymap.doubleClickZoom.disable();
                mymap.on('dblclick', function(event) {
	                mymap.setView(event.latlng, mymap.getZoom()+1);
                });
                mymap.on('move', function(event) {
                    cercle_centre.setLatLng(event.target.getCenter());
                });
                L.control.zoom({ position: 'bottomright' }).addTo(this);
                $('.leaflet-control-zoom-in').addClass('fr-btn  fr-icon-zoom-in-line').empty();
                $('.leaflet-control-zoom-out').addClass('fr-btn  fr-icon-zoom-out-line').empty();
            }")
  })
  
  observe({
    leafletProxy("map",
                 data = df()[,c("Longitude", "Latitude")]) %>%
      clearGroup("etabs") %>%
      addCircles(lng = ~Longitude,
                 lat = ~Latitude,
                 radius = 5 ,
                 stroke = FALSE,
                 fillOpacity = 0.8,
                 group = "etabs")
  })
  
  observe({
    if (taille() > TAILLE_MAX) {
      shiny::showModal(
        shiny::modalDialog(title = "Taille maximale",
                           "La taille maximale est de 10 km !",
                           easyClose = TRUE,
                           footer = NULL),
        session)
      updateNumericInput_dsfr(inputId = "taille", value = TAILLE_MAX)
    } else session$sendCustomMessage("taille", taille())
  })
  
  output$tbl <- DT::renderDT(
    datatable(
      df(),
      extensions = c("Scroller", "Buttons"),
      callback = JS(
        "table.on('init', function() {
          $('div.has-feedback input[type=search]').attr('placeholder', 'Filtrer');
          $('.dataTables_scrollBody').addClass('fr-table');
          $('#boutons').empty().append($('div.dt-buttons'));
          $('div.dt-buttons').children().removeAttr('class').addClass('fr-btn fr-p1-w');
          $('#filtre').empty().append($('.dataTables_filter'));
          $('.dataTables_filter > label > input[type=search]').addClass('fr-input');
        });
        "
      ),
      selection = "none",
      rownames = FALSE,
      filter = "top",
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '80px', targets = c(7L,8L))),
        dom = 'Bfrtip',
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = 350,
        scroller = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json",
          search = ""
          
        ),
        buttons = list(
          list(
            extend = "copy",
            charset = "utf-8",
            bom = TRUE,
            exportOptions = list(
              modifier = list(
                search = "applied"
              )
            )
          ),
          list(
            extend = "csv",
            charset = "utf-8",
            bom = TRUE,
            exportOptions = list(
              modifier = list(
                search = "applied"
              )
            )
          ),
          list(
            extend = "excel",
            charset = "utf-8",
            exportOptions = list(
              modifier = list(
                search = "applied"
              )
            )
          )
        )
      )
    )
  )
}

shinyApp(ui, server)
