library(RSQLite)
library(here)
library(dplyr)
library(readr)
library(dbplyr)
library(stringr)

unlink(here("retraitement", "sqlite"), recursive = TRUE)
Sys.sleep(5L)
dir.create(here("retraitement", "sqlite"), showWarnings = FALSE)

con <- dbConnect(RSQLite::SQLite(), here("retraitement", "sqlite", "db.sqlite"),
                 extended_types = TRUE)

dbExecute(con,
          "CREATE TABLE stock_ent_idf(
          siren TEXT not null,
          denominationUniteLegale TEXT,
          nomUsageUniteLegale TEXT,
          prenomUsuelUniteLegale TEXT,
          sexeUniteLegale TEXT,
          trancheEffectifsUniteLegale TEXT,
          primary key (siren))")

dbExecute(con,
          "CREATE TABLE stock_etabs_geoloc_idf(
          siret TEXT not null,
          siren TEXT not null,
          trancheEffectifsEtablissement TEXT,
          activitePrincipaleEtablissement TEXT,
          dateCreationEtablissement DATE,
          codePostalEtablissement TEXT,
          libelleCommuneEtablissement TEXT,
          numeroVoieEtablissement TEXT,
          typeVoieEtablissement TEXT,
          libelleVoieEtablissement TEXT,
          enseigneEtablissement TEXT,
          x_longitude REAL,
          y_latitude REAL,
          primary key (siret),
          foreign key (siren) REFERENCES stock_ent_idf (siren))")

dbDisconnect(con)
