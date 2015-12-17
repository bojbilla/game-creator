#!/bin/sh
cd ..
sbt assembly
cp target/scala-2.11/game-creator.jar deployment/
