FROM inwt/r-shiny:4.0.1

RUN Rscript -e "devtools::install_github('INWTlab/shiny-matrix')"

ENV PKG OsteoBioR

COPY . .

RUN installPackage

CMD ["Rscript", "-e", "OsteoBioR::startApplication(3838)"]