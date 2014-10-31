#-*- mode:conf; -*-

FROM docker.knewton.net/knewton/check-graph:dev
MAINTAINER Tim Dysinger <tim@knewton.com>

# BUILD
ADD ./ /usr/local/src/check-graph/
WORKDIR /usr/local/src/check-graph
RUN cabal install --global --prefix=/usr/local
RUN strip /usr/local/bin/check-graph
