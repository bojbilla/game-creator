package me.reminisce.database

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, WordSpecLike}
import reactivemongo.api.DefaultDB
import reactivemongo.bson.BSONInteger
import reactivemongo.core.commands.GetLastError

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

abstract class DatabaseTester(actorSystemName: String) extends TestKit(ActorSystem(actorSystemName, ConfigFactory.parseString("akka.loglevel = ERROR")))
with ImplicitSender
with WordSpecLike with BeforeAndAfterAll with BeforeAndAfterEach {

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
  }

  override def afterEach(): Unit = {
    Thread.sleep(100)
  }

  protected def testWithDb(test: DefaultDB => Unit): Unit = {
    val dbId = Random.nextInt // if the actorSystemName is shared for unknown reasons.
    val connection = DatabaseTestHelper.getConnection
    val dbName = s"DB${dbId}_for$actorSystemName"

    connection.database(dbName).onComplete {
      case Success(db) =>
        test(db)
        db.drop()
      case Failure(e) =>
        fail(s"${e.getMessage}")
    }
  }
}