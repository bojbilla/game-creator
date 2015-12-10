FROM java:openjdk-7-jre

LABEL version 1.0
LABEL description "Game logic of reminisce.me."

ENV REMINISCEME_FOLDER /home/gc_user/reminisce.me
ENV GAME_CREATOR_HOST 0.0.0.0
ENV MONGODB_HOST mongo
ENV "GAME_CREATOR_MODE" "PROD"
EXPOSE 9900

RUN groupadd -r gc_group && useradd -r -g gc_group gc_user

RUN mkdir -p $REMINISCEME_FOLDER
WORKDIR $REMINISCEME_FOLDER
RUN chown -R gc_user:gc_group .
USER gc_user

ADD game-creator.jar game-creator.jar

CMD java -Xmx128m -jar game-creator.jar
