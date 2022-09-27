FROM r-base:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libcurl4-openssl-dev \
        libfontconfig1-dev \
        libxml2-dev \
        libssl-dev \
        sqlite3 \
        xml2 \
        qpdf \
&& rm -rf /var/lib/apt/lists/*


RUN Rscript -e "install.packages(c('mlr3','devtools','randomForest'))"

COPY main.r /usr/local/bin/
COPY model /usr/local/bin/model
RUN chmod a+x /usr/local/bin/main.r
COPY mlr3extralearners-0.5.48 /usr/local/bin/mlr3extra
RUN Rscript -e "install.packages(c('dplyr','scales'))"
RUN Rscript -e "devtools::install_local('/usr/local/bin/mlr3extra')"

ENTRYPOINT ["Rscript", "/usr/local/bin/main.r"]
