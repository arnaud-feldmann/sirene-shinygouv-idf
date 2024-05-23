library(RSQLite)
library(here)

unlink(here("..", "app", "sqlite"), recursive = TRUE)
Sys.sleep(5L)
dir.create(here("..", "app", "sqlite"), showWarnings = FALSE)

con <- dbConnect(RSQLite::SQLite(), here("..", "app", "sqlite", "db.sqlite"),
                 extended_types = TRUE)

dbExecute(con,
          "CREATE TABLE stock_ent_idf(
          siren TEXT not null,
          nicSiegeUniteLegale not null,
          denominationUniteLegale TEXT,
          trancheEffectifsUniteLegale TEXT,
          categorieJuridiqueUniteLegale TEXT,
          economieSocialeSolidaireUniteLegale TEXT,
          denominationUsuelleUniteLegale TEXT,
          primary key (siren),
          foreign key (siren, nicSiegeUniteLegale) REFERENCES stock_etab_idf (siren, nic))")

dbExecute(con,
          "CREATE TABLE stock_etabs_geoloc_idf(
          siren TEXT not null,
          nic TEXT not null,
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
          denominationUsuelleEtablissement TEXT,
          primary key (siren, nic),
          foreign key (siren) REFERENCES stock_ent_idf (siren))")

dbDisconnect(con)
