package me.reminisce.database

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}
import reactivemongo.api.DefaultDB
import reactivemongo.bson.BSONInteger
import reactivemongo.core.commands.GetLastError

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

abstract class DatabaseTester(actorSystemName: String) extends TestKit(ActorSystem(actorSystemName, ConfigFactory.parseString("akka.loglevel = ERROR")))
with ImplicitSender
with WordSpecLike with BeforeAndAfterAll {

  val safeLastError = new GetLastError(w = Some(BSONInteger(1)))
  val dbs = mutable.Set[DefaultDB]()

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    dbs.foreach(_.drop())
  }

  protected def newDb(): DefaultDB = {
    val dbId = Random.nextInt // if the actorSystemName is shared for unknown reasons.
    val connection = DatabaseTestHelper.getConnection
    val db = connection(s"DB${dbId}_for$actorSystemName")
    dbs += db
    db
  }
}