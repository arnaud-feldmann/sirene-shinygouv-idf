library(RSQLite)
library(here)
library(dplyr)
library(readr)
library(dbplyr)
library(stringr)

# Geoloc Sirene (https://www.data.gouv.fr/fr/datasets/geolocalisation-des-etablissements-du-repertoire-sirene-pour-les-etudes-statistiques/)
geoloc_adresse <- here("input", "GeolocalisationEtablissement_Sirene_pour_etudes_statistiques_utf8.csv")

# Sirene stock etab (https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/)
stock_etabs_adresse <- here("input", "StockEtablissement_utf8.csv")

unlink(here("retraitement", "sqlite"), recursive = TRUE)
dir.create(here("retraitement", "sqlite"), showWarnings = FALSE)
con <- dbConnect(RSQLite::SQLite(), here("retraitement", "sqlite", "db.sqlite"))

geoloc_adresse %>%
  read_delim_chunked(delim = ";",
                     col_types = cols_only(siret = col_character(),
                                           x_longitude = col_double(),
                                           y_latitude = col_double(),
                                           plg_code_commune = col_character()),
                     chunk_size = 1000000L,
                     callback = function(tbl, pos) {
                       tbl %>%
                         filter(str_sub(plg_code_commune, 1L, 2L) %in% c("75", "77", "78", "91", "92", "93", "94", "95")) %>%
                         select(siret, x_longitude, y_latitude) %>%
                         { dbWriteTable(conn = con, name = "geoloc_idf", value = .,
                                        append = TRUE) }
                     }
  )

stock_etabs_adresse %>%
  read_csv_chunked(col_types = cols_only(siret = col_character(),
                                         trancheEffectifsEtablissement = col_character(),
                                         activitePrincipaleEtablissement = col_character(),
                                         dateCreationEtablissement = col_date(format = "%Y-%m-%d"),
                                         codePostalEtablissement = col_character(),
                                         numeroVoieEtablissement = col_character(),
                                         typeVoieEtablissement = col_character(),
                                         libelleVoieEtablissement = col_character(),
                                         etatAdministratifEtablissement = col_character()),
                   chunk_size = 1000000L,
                   callback = function(tbl, pos) {
                     tbl %>%
                       filter((is.na(etatAdministratifEtablissement) | etatAdministratifEtablissement == "A") &
                                str_sub(codePostalEtablissement, 1L, 2L) %in% c("75", "77", "78", "91", "92", "93", "94", "95")) %>%
                       select(-etatAdministratifEtablissement) %>%
                       { dbWriteTable(conn = con, name = "stock_etabs_idf", value = .,
                                      append = TRUE) }
                   })

dbExecute(
  con,
  "CREATE TABLE stock_etabs_geoloc_idf(
    siret TEXT not null,
    trancheEffectifsEtablissement TEXT,
    activitePrincipaleEtablissement TEXT,
    dateCreationEtablissement TEXT,
    codePostalEtablissement TEXT,
    numeroVoieEtablissement TEXT,
    typeVoieEtablissement TEXT,
    libelleVoieEtablissement TEXT,
    x_longitude REAL,
    y_latitude REAL,
    primary key (siret)
  )"
)
dbExecute(
  con,
  "INSERT INTO stock_etabs_geoloc_idf 
  SELECT s.siret, s.trancheEffectifsEtablissement, s.activitePrincipaleEtablissement,
  s.dateCreationEtablissement, s.codePostalEtablissement, s.numeroVoieEtablissement,
  s.typeVoieEtablissement, s.libelleVoieEtablissement, g.x_longitude, g.y_latitude
  FROM geoloc_idf as g, stock_etabs_idf as s
  WHERE g.siret = s.siret"
)
dbExecute(con,
          "CREATE INDEX idx_x_longitude
          ON stock_etabs_geoloc_idf(x_longitude)")
dbExecute(con,
          "CREATE INDEX idx_y_latitude
          ON stock_etabs_geoloc_idf(y_latitude)")
dbExecute(con,
          "CREATE INDEX idx_activitePrincipaleEtablissement
          ON stock_etabs_geoloc_idf(activitePrincipaleEtablissement)")
dbExecute(con,
          "CREATE INDEX idx_trancheEffectifsEtablissement
          ON stock_etabs_geoloc_idf(trancheEffectifsEtablissement)")
dbExecute(con,
          "DROP TABLE geoloc_idf")
dbExecute(con,
          "DROP TABLE stock_etabs_idf")
dbDisconnect(con)
