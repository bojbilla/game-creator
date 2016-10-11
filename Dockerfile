FROM reminisceme/game-creator:base

COPY . game-creator/
RUN cd game-creator && sbt update && sbt assembly
RUN cp game-creator/target/scala-2.11/game-creator.jar .
RUN chown -R gc_user:gc_group .
USER gc_user

CMD java -Xmx128m -jar game-creator.jar
