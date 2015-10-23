package me.reminisce.database

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.TestsConfig
import org.scalatest._
import reactivemongo.api.{DefaultDB, MongoDriver}

object DatabaseTestHelper extends MongoEmbedDatabase {

  import scala.concurrent.ExecutionContext.Implicits.global

  var portsInUse: Set[Int] = Set()


  private var db: DefaultDB = null
  private var port = 0
  private var mongoProps: MongodProps = null
  private var driver: MongoDriver = null

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


  def getDb: DefaultDB = {
    this.synchronized {
      if (db == null) {
        port = getNewPort
        mongoProps = mongoStart(port = port)
        driver = new MongoDriver
        val connection = driver.connection(s"localhost:$port" :: Nil)
        db = connection("mydb")
      }
      db
    }
  }

  def closeDb() = {
    this.synchronized {
      if (db != null) {
        mongoStop(mongoProps)
        driver.system.shutdown()
        releasePort(port)
        port = 0
        mongoProps = null
        driver = null
        db = null
      }
    }
  }
}

abstract class DatabaseTester(actorSystemName: String) extends TestKit(ActorSystem(actorSystemName, ConfigFactory.parseString("akka.loglevel = OFF")))
with ImplicitSender
with WordSpecLike with BeforeAndAfterAll with BeforeAndAfterEach {

  import scala.concurrent.ExecutionContext.Implicits.global

  val db = DatabaseTestHelper.getDb

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
  }

  override def afterEach(): Unit = {
    db.drop()
  }
}