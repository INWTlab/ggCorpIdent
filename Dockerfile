FROM inwt/r-batch:3.5.1

ADD . .

RUN apt-get -y update \
    && apt-get install -y --no-install-recommends \
        libpng-dev \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/* \
    && installPackage
