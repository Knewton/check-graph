#-*- mode:conf; -*-

FROM knewton/check-graph:lib
MAINTAINER Tim Dysinger <tim@knewton.com>

ADD .cabal-sandbox/bin/check-graph /usr/local/bin/check-graph
