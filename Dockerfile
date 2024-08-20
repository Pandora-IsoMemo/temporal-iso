FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN installPackage

RUN apt-get update && apt-get install -y \
    jags \
    && installPackage

CMD ["Rscript", "-e", "OsteoBioR::startApplication(3838)"]
