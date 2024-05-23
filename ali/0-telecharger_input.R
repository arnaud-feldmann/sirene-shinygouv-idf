library(here)

temp_dir <- tempdir()
geoloc <- file.path(temp_dir,"GeolocalisationEtablissement_Sirene_pour_etudes_statistiques_utf8.zip")
etabs <- file.path(temp_dir,"StockEtablissement_utf8.zip")
ent <- file.path(temp_dir,"StockUniteLegale_utf8.zip")

# Geoloc Sirene (https://www.data.gouv.fr/fr/datasets/geolocalisation-des-etablissements-du-repertoire-sirene-pour-les-etudes-statistiques/)
download.file("https://www.data.gouv.fr/fr/datasets/r/ba6a4e4c-aac6-4764-bbd2-f80ae345afc5",
              geoloc,
              method = "wget",
              quiet = TRUE)
unzip(geoloc,
      exdir = here("input"))

# Sirene stock etab (https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/)
download.file("https://www.data.gouv.fr/fr/datasets/r/0651fb76-bcf3-4f6a-a38d-bc04fa708576",
              etabs,
              method = "wget",
              quiet = TRUE)
unzip(etabs,
      exdir = here("input"))


# Sirene stock entreprises (https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/)
download.file("https://www.data.gouv.fr/fr/datasets/r/825f4199-cadd-486c-ac46-a65a8ea1a047",
              ent,
              method = "wget",
              quiet = TRUE)
unzip(ent,
      exdir = here("input"))
