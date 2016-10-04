
#Game Creator for reminisce.me

[![Build Status](https://travis-ci.org/reminisceme/game-creator.svg?branch=master)](https://travis-ci.org/reminisceme/game-creator)
[![Codacy Badge](https://api.codacy.com/project/badge/grade/a1618ddd953a48339f3b9765e9e9ebd5)](https://www.codacy.com/app/reminisceme/game-creator)
[![Codacy Badge](https://api.codacy.com/project/badge/coverage/a1618ddd953a48339f3b9765e9e9ebd5)](https://www.codacy.com/app/reminisceme/game-creator)

The game-creator fetches the user data from Facebook, and uses the obtained data to create a game board.

For further details please refer to the [wiki](https://github.com/reminisceme/game-creator/wiki).

## Installation

1. Install SBT.

  See the [official documentation](http://www.scala-sbt.org/0.13/docs/Setup.html).

2. Install MongoDB.

  See the [official documentation](https://docs.mongodb.com/manual/installation/).

3. (Optional) Change the value of the following environment variables:

  ```bash
  $ export GAME_CREATOR_MODE=[DEV|PROD]
  ```
  This is the application mode, in `DEV` mode the `/dropDatabase` route becomes available (see the [wiki](https://github.com/reminisceme/game-creator/wiki/Implemented-Functionalities#droping-the-database) ).Default value is `DEV`.

  ```bash
  $ export GAME_CREATOR_HOST=...
  ```
  Host the game creator has to bind itself on. Default value is `localhost`.

  ```bash
  $ export GAME_CREATOR_PORT=...
  ```
  Port the game creator has to bind itself on. Default value is `9900`.

  ```bash
  $ export MONGODB_HOST=...
  ```
  Host of the mongo database, by default it takes the value provided in `GAME_CREATOR_HOST`.

  ```bash
  $ export REMINISCE_MONGO_DB=...
  ```
  Name of the mongo database to use. Default value is `game_creator`.

4. Run mongo:

  ```bash
  $ mongod --dbpath /path/to/db
  ```
  The default path is `/data/db`.

  Instead, you can start the mongod service:
  ```bash
  $ systemctl start mongod
  ```

5. Run the project with 
  ```bash
  $ sbt run
  ```

## Tests

You can run the tests with:

```bash
$ sbt test
```
