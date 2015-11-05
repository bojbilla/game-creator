package me.reminisce.database

import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import reactivemongo.api.{MongoConnection, MongoDriver}

object DatabaseTestHelper extends MongoEmbedDatabase {

  // does not conflict with live mongo instances
  private val port = 28000
  private val mongoProps: MongodProps = mongoStart(port = port)
  private lazy val driver: MongoDriver = new MongoDriver
  private lazy val connection: MongoConnection = driver.connection(s"localhost:$port" :: Nil)

  def closeConnection() = {
    this.synchronized {
      mongoStop(mongoProps)
      driver.system.shutdown()
    }
  }

  def getConnection: MongoConnection = {
    this.synchronized {
      connection
    }
  }
}

