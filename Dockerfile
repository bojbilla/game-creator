FROM reminisceme/scala-base:latest

ENV REMINISCEME_FOLDER /home/gc_user/reminisce.me
ENV GAME_CREATOR_HOST 0.0.0.0
ENV MONGODB_HOST mongo
ENV "GAME_CREATOR_MODE" "PROD"
EXPOSE 9900

RUN groupadd -r gc_group && useradd -r -g gc_group gc_user

RUN mkdir -p $REMINISCEME_FOLDER
WORKDIR $REMINISCEME_FOLDER
COPY . game-creator/
RUN cd game-creator && sbt update && sbt assembly
RUN cp game-creator/target/scala-2.11/game-creator.jar .
RUN rm -rf game-creator/
RUN chown -R gc_user:gc_group .
USER gc_user

CMD java -Xmx128m -jar game-creator.jar
