#!/bin/sh
cd ..
sbt assembly
cp target/scala-2.10/game-creator.jar deployment/
