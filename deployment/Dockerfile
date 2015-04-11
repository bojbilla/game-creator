FROM ubuntu:14.04

# add apt repository for mongodb
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
RUN echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | tee /etc/apt/sources.list.d/10gen.list

# mandatory apt update and useful package installation
RUN apt-get update && apt-get install -y default-jre-headless 
RUN apt-get install -y mongodb-org

# mongodb database location
RUN mkdir -p /data/db

ADD game-creator.jar game-creator.jar

# Enable the game creator to listen to the outside world
ENV GAME_CREATOR_HOST 0.0.0.0

# setup for the run script to work
ENV REMINISCEME_FOLDER /reminisce.me
RUN mkdir $REMINISCEME_FOLDER && mv game-creator.jar $REMINISCEME_FOLDER
ADD run.sh run.sh
RUN chmod +x run.sh

# exposed port to reach the game-creator
EXPOSE 9900

CMD ["sh", "run.sh"]
