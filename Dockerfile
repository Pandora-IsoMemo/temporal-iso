FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "OsteoBioR::startApplication(3838)"]
