library(shiny)
library(leaflet)
library(RSQLite)
library(here)
library(DT)
library(sp)
library(shinyWidgets)
library(htmlwidgets)
#remotes::install_github("spyrales/shinygouv")
library(shinygouv)

args <- commandArgs(trailingOnly = TRUE)
host <- if (length(args) == 0L) getOption("shiny.host", "127.0.0.1") else args[1L]
port <- if (length(args) == 0L) getOption("shiny.port") else as.integer(args[2L])

`%||%` <- function (x, y) if (is.null(x)) y else x

list_a88_a17 <- readRDS(here("tables", "list_a88_a17.Rds"))
df_a88_a17 <- readRDS(here("tables", "df_a88_a17.Rds"))
list_tranches <- readRDS(here("tables", "list_tranches.Rds"))
df_tranches <- readRDS(here("tables", "df_tranches.Rds"))

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

get_query <- function(con,
                      center = CENTRE_DEFAUT, taille = TAILLE_DEFAUT) {
  X_VOISINAGE <- taille / MULTIPLE_ANGLE_X
  Y_VOISINAGE <- taille / MULTIPLE_ANGLE_Y
  pq <- dbSendQuery(con,
                    paste(
                      "SELECT etab.enseigneEtablissement, etab.trancheEffectifsEtablissement, ",
                      "etab.dateCreationEtablissement, etab.denominationUsuelleEtablissement, ",
                      "etab.x_longitude, etab.y_latitude, ",
                      "ent.denominationUniteLegale, ent.trancheEffectifsUniteLegale, ",
                      "ent.categorieJuridiqueUniteLegale, ent.economieSocialeSolidaireUniteLegale, ",
                      "ent.nicSiegeUniteLegale, ent.denominationUsuelleUniteLegale, ",
                      "SUBSTR(etab.activitePrincipaleEtablissement, 1, 2) as A88, ",
                      "etab.siren || etab.nic as siret, ",
                      "etab.siren || ent.nicSiegeUniteLegale as siret_siege, ",
                      "etab.numeroVoieEtablissement || ' ' || etab.typeVoieEtablissement || ' ' || ",
                      "etab.libelleVoieEtablissement || ' ' || etab.codePostalEtablissement || ",
                      "' ' || etab.libelleCommuneEtablissement as adresse ",
                      "FROM stock_etabs_geoloc_idf as etab, stock_ent_idf as ent, A88_TEMP as s1, TRA_TEMP as s2 ",
                      "WHERE A88 = s1.sel AND trancheEffectifsEtablissement = s2.sel ",
                      "AND x_longitude BETWEEN ? AND ? ",
                      "AND y_latitude BETWEEN ? AND ? ",
                      "AND etab.siren = ent.siren"))
  res <- dbBind(pq, list(center[1L] - X_VOISINAGE,
                         center[1L] + X_VOISINAGE,
                         center[2L] - Y_VOISINAGE,
                         center[2L] + Y_VOISINAGE))
  res <- dbFetch(res)
  res$distance_point <-
    spDistsN1(pts = cbind(res$x_longitude, res$y_latitude),
              pt = center,
              longlat = TRUE)
  res <- res[res$distance_point <= taille/1000,]
  res <- res[order(res$distance_point),]
  res <- head(res, TAILLE_MAX)
  res <- merge(x = res,
               y = df_tranches,
               by.x = "trancheEffectifsEtablissement",
               by.y = "tranche",
               all.x = TRUE)
  colnames(res)[colnames(res) == "tranche_lbl"] <- "tr_etab"
  res <- merge(x = res,
               y = df_tranches,
               by.x = "trancheEffectifsUniteLegale",
               by.y = "tranche",
               all.x = TRUE)
  colnames(res)[colnames(res) == "tranche_lbl"] <- "tr_ent"
  res <- merge(x = res,
               y = df_a88_a17,
               by = "A88",
               all.x = TRUE)
  res <- res[, c("siret", "siret_siege", "enseigneEtablissement",
                 "denominationUniteLegale", "A88_lbl",
                 "tr_etab", "tr_ent", "adresse", "categorieJuridiqueUniteLegale",
                 "economieSocialeSolidaireUniteLegale",
                 "dateCreationEtablissement", "denominationUsuelleEtablissement",
                 "denominationUsuelleUniteLegale", "x_longitude", "y_latitude",
                 "distance_point")]
  colnames(res) <- c("SIRET", "SIRET (siège)", "Nom d'établissement", "Nom d'entreprise",
                     "Secteur", "Tranche d'effectifs (établissement)",
                     "Tranche d'effectifs (entreprise)", "Adresse",
                     "CJ", "Economie Sociale et Solidaire", "Date de création",
                     "Dénomination usuelle (établissement)",
                     "Dénomination usuelle (entreprise)",
                     "Longitude", "Latitude",
                     "Distance au point")
  dbClearResult(pq)
  res
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
                         tags$script("
                         cercle_centre = null;
                         Shiny.addCustomMessageHandler('taille', function(taille) {
                          if (cercle_centre !== null) {
                            cercle_centre.setRadius(taille);
                          }
                         });
                         $(document).bind('scroll',function () {
                           window.scrollTo(0,0);
                         });"
                         ),
                         includeScript("centercross.js")
                       ),
                       leafletOutput("map", width = "100%", height = "100%"),
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = FALSE,
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
                                         position = "right",
                                         optionHeight = "30px | 40px",
                                         optionsSelectedText = "options sélectionnées"
                                       )
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
                                         position = "right",
                                         optionHeight = "30px | 40px",
                                         optionsSelectedText = "options sélectionnées"
                                       )
                                     ),
                                     actionButton_dsfr("actualiser_map", "Go !"),
                                     div(textOutput("position"), id = "position-conteneur")
                       ),
                       
                       div(id="cite",
                           'Source :', tags$em('Sirene (Insee)'))
                   )
                   
  ),
  navbarPanel_dsfr("Données",
                   div(
                     fluidRow_dsfr(
                       column_dsfr(3L,
                                   div(
                                     HTML("<label id='filtre-label' for='filtre'><b>Filtrer</b></label>"),
                                     div(id = "filtre"),
                                     id = "conteneur-filtre")
                       ),
                       column_dsfr(2L,div(
                         HTML("<label id='boutons-label' for='boutons'><b>Télécharger</b></label>"),
                         div(id = "boutons"),
                         id = "conteneur-boutons")
                       ),
                       column_dsfr(6L, div(
                         div(id = "paginfo"),
                         id = "conteneur-paginfo")),
                     ),
                     div(DTOutput("tbl"),
                         id = "conteneur-tbl")
                   ),
                   class = "fr-container--fluid"
  )
)

session_init <- function() {
  con <-
    dbConnect(RSQLite::SQLite(),
              here("sqlite", "db.sqlite"),
              flags = SQLITE_RO,
              extended_types = TRUE)
  dbWriteTable(con, "A88_TEMP", data.frame(sel = A88_SEL_DEFAUT), overwrite = TRUE, temporary = TRUE)
  dbWriteTable(con, "TRA_TEMP", data.frame(sel = TRANCHES_SEL_DEFAUT), overwrite = TRUE, temporary = TRUE)
  con
}

server <- function(input, output, session) {
  
  session$userData$con <- session_init()
  
  center <- reactive(c(input$map_center$lng, input$map_center$lat) %||% CENTRE_DEFAUT)
  taille <- reactive(input$taille %||% TAILLE_DEFAUT |> min(TAILLE_MAX + 1L))

  df <- reactiveVal(value = get_query(session$userData$con))
  
  observe({
    dbWriteTable(session$userData$con, "A88_TEMP", data.frame(sel = input$a88 %||% character()), overwrite = TRUE, temporary = TRUE)
  })
  observe({
    dbWriteTable(session$userData$con, "TRA_TEMP", data.frame(sel = input$tranches %||% character()), overwrite = TRUE, temporary = TRUE)
  })
  
  observeEvent(input$actualiser_map,
               {
                 center <- center()
                 taille <- taille()
                 df(get_query(session$userData$con, center, taille))
                 if (NROW(df()) >= 10000L) {
                   shiny::showModal(
                     shiny::modalDialog(title = "Trop de résultats !",
                                        "La recherche a plus de 10000 résultats, seuls les 10000 les plus proches ont été retenus",
                                        easyClose = TRUE,
                                        footer = NULL),
                     session)
                 }
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
    server = FALSE,
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
          $('#paginfo').empty().append($('.dataTables_info'));
          $('.dataTables_filter > label > input[type=search]').addClass('fr-input');
          $(window).on('resize', function () {
            table.columns.adjust();
          });
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

runApp(shinyApp(ui, server),
       host = host,
       port = port)
