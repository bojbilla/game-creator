FROM java:openjdk-7-jre

ENV REMINISCEME_FOLDER /reminisce.me
ENV GAME_CREATOR_HOST 0.0.0.0
ENV MONGODB_HOST mongo
ENV "GAME_CREATOR_MODE" "PROD"

RUN mkdir $REMINISCEME_FOLDER
WORKDIR $REMINISCEME_FOLDER
ADD game-creator.jar game-creator.jar

EXPOSE 9900

CMD java -Xmx128m -jar game-creator.jar >/var/log/game-creator.log 2>&1
