library(RSQLite)
library(here)
library(dplyr)
library(readr)
library(stringr)

# Geoloc Sirene (https://www.data.gouv.fr/fr/datasets/geolocalisation-des-etablissements-du-repertoire-sirene-pour-les-etudes-statistiques/)
geoloc_adresse <- here("input", "GeolocalisationEtablissement_Sirene_pour_etudes_statistiques_utf8.csv")

# Sirene stock entreprises (https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/)
stock_ent_adresse <- here("input", "StockUniteLegale_utf8.csv")

con <- dbConnect(RSQLite::SQLite(), here("app", "sqlite", "db.sqlite"),
                 extended_types = TRUE)

dbExecute(con, "DELETE FROM stock_ent_idf")

stock_ent_adresse %>%
  read_csv_chunked(col_types = cols_only(siren = col_character(),
                                         nicSiegeUniteLegale = col_character(),
                                         denominationUniteLegale = col_character(),
                                         trancheEffectifsUniteLegale = col_character(),
                                         etatAdministratifUniteLegale = col_character(),
                                         categorieJuridiqueUniteLegale = col_character(),
                                         economieSocialeSolidaireUniteLegale = col_character(),
                                         denominationUsuelle1UniteLegale = col_character(),
                                         denominationUsuelle2UniteLegale = col_character(),
                                         denominationUsuelle3UniteLegale = col_character()),
                   chunk_size = 1000000L,
                   callback = function(tbl, pos) {
                     tbl %>%
                       filter((is.na(etatAdministratifUniteLegale) | etatAdministratifUniteLegale == "A") &
                                trancheEffectifsUniteLegale != "NN") %>%
                       mutate(denominationUsuelleUniteLegale = str_c(denominationUsuelle1UniteLegale,
                                                                     denominationUsuelle2UniteLegale,
                                                                     denominationUsuelle3UniteLegale)) %>%
                       select(-c(etatAdministratifUniteLegale,
                                 denominationUsuelle1UniteLegale,
                                 denominationUsuelle2UniteLegale,
                                 denominationUsuelle3UniteLegale)) %>%
                       { dbAppendTable(conn = con, name = "stock_ent_idf", value = .) }
                   })
dbDisconnect(con)
