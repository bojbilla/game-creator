package me.reminisce.server

import scala.util.Properties._


object ApplicationConfiguration {

  // Development ? Release ?
  // For now, only the database wiping is prohibited when not in DEV mode
  val appMode = envOrElse("GAME_CREATOR_MODE", "DEV")

  val hostName = envOrElse("GAME_CREATOR_HOST", "localhost")
  val serverPort = envOrElse("GAME_CREATOR_PORT", "9900").toInt

  val mongoHost = envOrElse("MONGODB_HOST", hostName)
  val mongodbName = envOrElse("REMINISCE_MONGO_DB", "mydb")

}
