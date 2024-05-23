library(RSQLite)
library(here)
library(dplyr)
library(readr)
library(stringr)

# Geoloc Sirene
geoloc_adresse <- here("input", "GeolocalisationEtablissement_Sirene_pour_etudes_statistiques_utf8.csv")

# Sirene stock etab
stock_etabs_adresse <- here("input", "StockEtablissement_utf8.csv")

con <- dbConnect(RSQLite::SQLite(), here("..", "app", "sqlite", "db.sqlite"),
                 extended_types = TRUE)

dbExecute(con, "DROP TABLE IF EXISTS geoloc_idf")
dbExecute(con, "DROP TABLE IF EXISTS stock_etabs_idf")
dbExecute(con, "DELETE FROM stock_etabs_geoloc_idf")

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
                                         siren = col_character(),
                                         nic = col_character(),
                                         trancheEffectifsEtablissement = col_character(),
                                         activitePrincipaleEtablissement = col_character(),
                                         dateCreationEtablissement = col_date(format = "%Y-%m-%d"),
                                         codePostalEtablissement = col_character(),
                                         libelleCommuneEtablissement = col_character(),
                                         numeroVoieEtablissement = col_character(),
                                         typeVoieEtablissement = col_character(),
                                         libelleVoieEtablissement = col_character(),
                                         etatAdministratifEtablissement = col_character(),
                                         enseigne1Etablissement = col_character(),
                                         enseigne2Etablissement = col_character(),
                                         enseigne3Etablissement = col_character(),
                                         denominationUsuelleEtablissement = col_character()),
                   chunk_size = 1000000L,
                   callback = function(tbl, pos) {
                     tbl %>%
                       filter((is.na(etatAdministratifEtablissement) | etatAdministratifEtablissement == "A") &
                                trancheEffectifsEtablissement != "NN" &
                                str_sub(codePostalEtablissement, 1L, 2L) %in% c("75", "77", "78", "91", "92", "93", "94", "95")) %>%
                       mutate(across(starts_with("enseigne"), ~if_else(is.na(.x),"",.x))) %>%
                       mutate(enseigneEtablissement = str_c(enseigne1Etablissement,
                                                            enseigne2Etablissement,
                                                            enseigne3Etablissement)) %>%
                       select(-c(etatAdministratifEtablissement,
                                 enseigne1Etablissement,
                                 enseigne2Etablissement,
                                 enseigne3Etablissement)) %>%
                       { dbWriteTable(conn = con, name = "stock_etabs_idf", value = .,
                                      append = TRUE) }
                   })

dbBegin(con)
tryCatch(
  {
    dbExecute(con,
    "INSERT INTO stock_etabs_geoloc_idf 
    SELECT s.siren, s.nic, s.trancheEffectifsEtablissement, s.activitePrincipaleEtablissement,
    s.dateCreationEtablissement, s.codePostalEtablissement, s.libelleCommuneEtablissement,
    s.numeroVoieEtablissement,
    s.typeVoieEtablissement, s.libelleVoieEtablissement, s.enseigneEtablissement,
    g.x_longitude, g.y_latitude, s.denominationUsuelleEtablissement
    FROM geoloc_idf as g, stock_etabs_idf as s
    WHERE g.siret = s.siret"
    )
    dbExecute(con,
              "CREATE INDEX idx_query ON stock_etabs_geoloc_idf(SUBSTR(activitePrincipaleEtablissement, 1, 2), y_latitude, x_longitude, trancheEffectifsEtablissement)")
    dbExecute(con,
    "DROP TABLE geoloc_idf")
    dbExecute(con,
    "DROP TABLE stock_etabs_idf")
    dbExecute(con,
    "DELETE FROM stock_ent_idf as ent
    WHERE NOT EXISTS (SELECT '' FROM stock_etabs_geoloc_idf as etab 
    WHERE etab.siren = ent.siren)")
    dbCommit(con)
  },
  error = function(e) dbRollback(con)
)

dbExecute(con, "PRAGMA vacuum")
dbExecute(con, "PRAGMA optimize")

dbDisconnect(con)
