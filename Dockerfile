FROM rhub/r-minimal as ALI_RENV
WORKDIR /ali
RUN mkdir -p renv
COPY .Rprofile renv.lock shiny.Rproj ./
COPY renv/activate.R renv/settings.json ./renv/
RUN apk add build-base && installr -d renv && R -e "renv::restore()" && R -e "renv::isolate()"

FROM rhub/r-minimal as APP_RENV
WORKDIR /app
RUN mkdir -p renv
COPY ./app/.Rprofile ./app/renv.lock ./app/app.Rproj ./
COPY ./app/renv/activate.R ./app/renv/settings.json ./renv/
RUN apk add build-base gdal-dev libxml2-dev proj-dev gfortran geos-dev libpng-dev && installr -d renv && R -e "renv::restore()"
RUN R -e "renv::isolate()"

FROM rhub/r-minimal as ALI_EXEC
WORKDIR /ali
COPY --from=ALI_RENV /ali .
COPY ./0-telecharger_input.R ./1-definition_schemas.R ./2-alimentation_entreprises.R ./3-alimentation_etabs.R ./4-tables.R ./
RUN mkdir -p app/tables && mkdir app/sqlite && mkdir input
COPY ./input/a17_a88t.csv ./input/a17_a88t.csv
RUN Rscript 0-telecharger_input.R && Rscript 1-definition_schemas.R && Rscript 2-alimentation_entreprises.R && Rscript 3-alimentation_etabs.R && Rscript 4-tables.R

FROM rhub/r-minimal as APP_EXEC
LABEL org.opencontainers.image.source="https://github.com/arnaud-feldmann/sirene-shinygouv-idf"
WORKDIR /app
COPY --from=APP_RENV /app .
COPY ./app/app.R ./app/centercross.js ./app/styles.css ./
RUN mkdir tables && mkdir sqlite
COPY --from=ALI_EXEC /ali/app/tables/df_a88_a17.Rds /ali/app/tables/list_a88_a17.Rds ali/app/tables/df_tranches.Rds /ali/app/tables/list_tranches.Rds ./tables/
COPY --from=ALI_EXEC /ali/app/sqlite/db.sqlite ./sqlite/
RUN apk add libxml2-dev
EXPOSE 3838	
CMD ["Rscript", "app.R", "0.0.0.0", "3838"] 
