FROM ubuntu:16.04

LABEL version 1.0
LABEL description "Game logic of reminisce.me."

RUN echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
RUN echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
RUN apt-get -qq update && apt-get -qq -y install apt-transport-https software-properties-common
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823
RUN add-apt-repository ppa:webupd8team/java
RUN apt-get -qq update
RUN apt-get -qq install -y oracle-java8-installer
RUN apt-get -qq install -y sbt git-core

ENV REMINISCEME_FOLDER /home/gc_user/reminisce.me
ENV GAME_CREATOR_HOST 0.0.0.0
ENV MONGODB_HOST mongo
ENV "GAME_CREATOR_MODE" "PROD"
EXPOSE 9900

RUN groupadd -r gc_group && useradd -r -g gc_group gc_user

RUN mkdir -p $REMINISCEME_FOLDER
WORKDIR $REMINISCEME_FOLDER
RUN git clone https://github.com/reminisceme/game-creator.git
RUN cd game-creator && sbt update

CMD echo "hello"
