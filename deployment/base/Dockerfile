FROM java:openjdk-7-jre

LABEL version 1.0
LABEL description "Game logic of reminisce.me."

RUN apt-get -qq update && apt-get -qq -y install apt-transport-https
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823
RUN apt-get -qq update && apt-get -qq install -y sbt git-core

ENV REMINISCEME_FOLDER /home/gc_user/reminisce.me
ENV GAME_CREATOR_HOST 0.0.0.0
ENV MONGODB_HOST mongo
ENV "GAME_CREATOR_MODE" "PROD"
EXPOSE 9900

RUN groupadd -r gc_group && useradd -r -g gc_group gc_user

RUN mkdir -p $REMINISCEME_FOLDER
WORKDIR $REMINISCEME_FOLDER
RUN git clone https://github.com/reminisceme/game-creator.git
RUN cd game-creator && sbt update && sbt test

CMD echo "hello"
