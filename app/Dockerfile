FROM rhub/r-minimal as APP_RENV
WORKDIR /app
RUN mkdir -p renv
COPY .Rprofile renv.lock app.Rproj ./
COPY renv/activate.R renv/settings.json ./renv/
RUN apk add build-base gdal-dev libxml2-dev proj-dev gfortran geos-dev libpng-dev && installr -d renv && R -e "renv::restore()"
RUN R -e "renv::isolate()"

FROM rhub/r-minimal as APP_EXEC
WORKDIR /app
COPY --from=APP_RENV /app .
COPY app.R centercross.js styles.css ./
RUN mkdir tables && mkdir sqlite
COPY tables/df_a88_a17.Rds tables/list_a88_a17.Rds tables/df_tranches.Rds tables/list_tranches.Rds ./tables/
COPY sqlite/db.sqlite ./sqlite/
RUN apk add libxml2-dev
CMD ["Rscript", "app.R", "0.0.0.0", "3838"] 
