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
import scala.util.Random

abstract class DatabaseTester(actorSystemName: String) extends TestKit(ActorSystem(actorSystemName, ConfigFactory.parseString("akka.loglevel = ERROR")))
with ImplicitSender
with WordSpecLike with BeforeAndAfterAll with BeforeAndAfterEach {

  val dbs = mutable.Set[DefaultDB]()

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    dbs.foreach(_.drop())
  }

  override def afterEach(): Unit = {
    Thread.sleep(100)
  }


  protected def newDb(): DefaultDB = {
    val dbId = Random.nextInt // if the actorSystemName is shared for unknown reasons.
    val connection = DatabaseTestHelper.getConnection
    val db = connection(s"DB${dbId}_for$actorSystemName")
    dbs += db
    db
  }
}