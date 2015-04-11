#!/bin/sh

mongod > /var/log/mongod.log &
java -Xmx128m -jar $REMINISCEME_FOLDER/game-creator.jar >/var/log/game-creator.log 2>&1
