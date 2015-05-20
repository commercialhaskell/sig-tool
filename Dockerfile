#-*- mode:conf; -*-

FROM ubuntu:14.04
MAINTAINER Tim Dysinger <tim@fpcomplete.com>

# LOCALES
ENV LANG en_US.UTF-8
RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales

# HASKELL
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN apt-add-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y cabal-install-1.20 ghc-7.8.4 happy-1.19.3 alex-3.1.4 libffi-dev zlib1g-dev
ENV PATH /opt/ghc/7.8.4/bin:/opt/cabal/1.20/bin:/opt/happy/1.19.3/bin:/opt/alex/3.1.4/bin:$PATH

# PROJECT
ADD ./ /usr/local/src/
WORKDIR /usr/local/src/
RUN apt-get install -y wget
RUN wget http://www.stackage.org/lts/cabal.config
RUN cabal update
RUN cabal install -j --global --prefix=/usr/local
WORKDIR /

# RUN
CMD sig
