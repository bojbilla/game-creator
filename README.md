game-creator
============


##How to install:

clone the repository to you local machine

navigate to the cloned direcotry.


in the terminal:


	sbt

this should download all the dependencies.


inside the sbt console:

	run
	
to run the application. It will show the ports where it binded to. The default port used it `9900` but can be modified by using the environement varibale:

	GAME_CREATOR_PORT

with the port value as desired

by default it uses `localhost` but it can be modified by the env. variable:

	GAME_CREATOR_HOST
	
with the desired host name

##Connection to the Crawler

The game creator will connect to the crawler server to let it fetch the data from facebook. For this it needs to know the host name and port of the crawler server.

By default it will use the host: `localhost` and port number `9000`


this can be modified by the following env. variables:

	CRAWLER_HOST
	CRAWLER_PORT
	
to instruct the game-creator to ask the crawler to fetch the data simply make a `GET` request to the path:

 	http://gamecreator_host:gamecreator_port/fetchData?user_id=xxxxxxx&access_token=xxxxxxx
 	
 	
 where the xxxxxxx should be replaced by the facebook user id and access_token.
 
 As example when using default values:
 
 	http://localhost:9900/fetchData?user_id=xxxxxxx&access_token=xxxxxxx
 	
 	
 This will create a `GET` request to the crawler which will in turn make a `POST` request to the game-creator once it has finished fetching the data.
 
 
##Retrieved Data

 The fetched data will be stored inside a mongo db. The mongodb database and host will use by default the same hostname as the `game-creator` host and as a database `mydb`
 
 This can be modified by defining env. variables:
 
 	MONGODB_HOST  //for the host name
 	REMINISCE_MONGO_DB //for the database name
 	
 
##Game Creation
 
Once the game-creator has received the data from the crawler, a game board can be obtained by making the following `GET` request:

	http://gamecreator_host:gamecreator_port/gameboard ?user_id=xxxxxxx

which will return a gameboard in json format (see backend for futher information)
 
 
