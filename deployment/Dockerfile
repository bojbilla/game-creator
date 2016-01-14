FROM gc_base_image:latest

WORKDIR game-creator
RUN git pull --rebase
RUN sbt update
RUN sbt assembly
WORKDIR $REMINISCEME_FOLDER
RUN cp game-creator/target/scala-2.11/game-creator.jar .
RUN chown -R gc_user:gc_group .
USER gc_user


CMD java -Xmx128m -jar game-creator.jar
