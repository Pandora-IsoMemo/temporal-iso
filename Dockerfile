FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN Rscript -e "rstantools::rstan_config()"

RUN installPackage

CMD ["Rscript", "-e", "OsteoBioR::startApplication(3838)"]
