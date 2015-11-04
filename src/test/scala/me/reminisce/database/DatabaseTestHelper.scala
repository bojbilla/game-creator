package me.reminisce.database

import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import me.reminisce.TestsConfig
import reactivemongo.api.{MongoConnection, MongoDriver}

object DatabaseTestHelper extends MongoEmbedDatabase {

  var portsInUse: Set[Int] = Set()


  private var port = 0
  private var mongoProps: Option[MongodProps] = None
  private var driver: Option[MongoDriver] = None
  private var connection: Option[MongoConnection] = None
  private var nextDBId = 0

  private def getNewPort: Int = {
    this.synchronized {
      while (portsInUse.size >= TestsConfig.maximumParallelEmbedMongoNumber) {
        this.wait()
      }
      val newPort = if (portsInUse.isEmpty) {
        28000 // does not conflict with live mongo instances
      } else {
        portsInUse.max + 1
      }
      portsInUse += newPort
      return newPort
    }
  }


  private def releasePort(port: Int): Unit = {
    this.synchronized {
      if (portsInUse.contains(port)) {
        portsInUse -= port
        this.notifyAll()
      }
    }
  }

  def closeConnection() = {
    this.synchronized {
      mongoProps.foreach(m => mongoStop(m))
      driver.foreach(d => d.system.shutdown())
      releasePort(port)
      port = 0
      mongoProps = None
      driver = None
    }
  }

  def getDBId = {
    this.synchronized {
      nextDBId += 1
      nextDBId
    }
  }

  def getConnection: MongoConnection = {
    this.synchronized {
      connection match {
        case Some(conn) => conn
        case None =>
          port = getNewPort
          mongoProps = Some(mongoStart(port = port))
          val tDriver = new MongoDriver
          driver = Some(tDriver)
          val tConnection = tDriver.connection(s"localhost:$port" :: Nil)
          connection = Some(tConnection)
          tConnection
      }
    }
  }
}

