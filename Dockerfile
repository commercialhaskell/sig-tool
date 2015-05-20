#-*- mode:conf; -*-

FROM ubuntu:14.04
MAINTAINER Tim Dysinger <tim@fpcomplete.com>

# APT
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get install -y net-tools
RUN echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
    | tee /etc/apt/apt.conf.d/02proxy

# CERTS
RUN apt-get install -y ca-certificates
RUN update-ca-certificates

# LOCALES
ENV LANG en_US.UTF-8
RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales

# HASKELL
ENV CABAL=1.20 GHC=7.8.4 HAPPY=1.19.5 ALEX=3.1.4
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN apt-add-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y cabal-install-$CABAL ghc-$GHC happy-$HAPPY alex-$ALEX libgmp10
ENV PATH /opt/ghc/$GHC/bin:/opt/cabal/$CABAL/bin:/opt/happy/$HAPPY/bin:/opt/alex/$ALEX/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN cabal update

# PROJECT
RUN apt-get install -y zlib1g zlib1g-dev wget
ADD ./ /usr/local/src/
WORKDIR /usr/local/src/
RUN cabal sandbox init --sandbox=/usr/local
RUN if [ $(echo $GHC|cut -d'.' -f2) = "10" ]; \
    then wget http://www.stackage.org/nightly/cabal.config; \
    else wget http://www.stackage.org/lts/cabal.config; \
    fi
RUN cabal install -j --global --prefix=/usr/local
WORKDIR /
RUN rm -rf /usr/local/src
ENTRYPOINT sig
CMD --help

# CLEANUP
ENV PATH /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN dpkg --purge cabal-install-$CABAL ghc-$GHC happy-$HAPPY alex-$ALEX
RUN dpkg --purge zlib1g-dev wget
RUN apt-get autoremove -y
RUN apt-get clean
RUN rm /etc/apt/apt.conf.d/02proxy
