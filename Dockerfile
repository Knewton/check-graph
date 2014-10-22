#-*- mode:conf; -*-

FROM ubuntu:12.04
MAINTAINER Tim Dysinger <tim@knewton.com>

# HASKELL
RUN apt-get update
RUN apt-get install -y python-software-properties
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
    llvm \
    zlib1g-dev
ENV PATH /opt/ghc/7.8.3/bin:/opt/cabal/1.20/bin:/opt/alex/3.1.3/bin:/opt/happy/1.19.4/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN cabal update

# BUILD
ADD ./ /usr/src/graph-check/
WORKDIR /usr/src/graph-check
RUN cabal install --enable-split-objs --ghc-options='-fllvm'
WORKDIR /
