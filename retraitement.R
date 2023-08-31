library(arrow)
library(here)
library(dplyr)

# Geoloc Sirene (https://www.data.gouv.fr/fr/datasets/geolocalisation-des-etablissements-du-repertoire-sirene-pour-les-etudes-statistiques/)
geoloc_adresse <- here("input", "GeolocalisationEtablissement_Sirene_pour_etudes_statistiques_utf8.csv")

# Sirene stock etab (https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/)
stock_etabs_adresse <- here("input", "StockEtablissement_utf8.csv")

geoloc_pqt <-
  geoloc_adresse %>%
  read_delim_arrow(col_select = c(siret, x_longitude, y_latitude, plg_code_commune),
                   delim = ";",
                   as_data_frame = FALSE) %>%
  filter(str_sub(plg_code_commune, 1L, 2L) %in% c("75", "77", "78", "91", "92", "93", "94", "95")) %>%
  select(siret, x_longitude, y_latitude) %>%
  compute()

stock_etabs_adresse %>%
  read_csv_arrow(col_select = c(siret,
                                trancheEffectifsEtablissement,
                                activitePrincipaleEtablissement,
                                dateCreationEtablissement,
                                codePostalEtablissement,
                                numeroVoieEtablissement,
                                typeVoieEtablissement,
                                libelleVoieEtablissement),
                 as_data_frame = FALSE) %>%
  inner_join(geoloc_pqt,
             by = "siret") %>%
  write_parquet(here("retraitement", "etabs.parquet"))

