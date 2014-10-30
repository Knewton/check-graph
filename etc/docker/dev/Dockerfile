#-*- mode:conf; -*-

FROM docker.knewton.net/knewton/check-graph:lib
MAINTAINER Tim Dysinger <tim@knewton.com>

# HASKELL
RUN apt-get update
RUN apt-get install -y software-properties-common ;# "python-software-properties" in older releases
RUN apt-add-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y \
    alex-3.1.3 \
    build-essential \
    cabal-install-1.20 \
    ghc-7.8.3 \
    happy-1.19.4 \
    libffi-dev \
    libgmp-dev \
    zlib1g-dev
ENV PATH /opt/ghc/7.8.3/bin:/opt/cabal/1.20/bin:/opt/alex/3.1.3/bin:/opt/happy/1.19.4/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# PROJECT
ADD ./ /usr/local/src/check-graph/
WORKDIR /usr/local/src/check-graph
RUN cabal update && cabal install --global --prefix=/usr/local --only-dependencies
