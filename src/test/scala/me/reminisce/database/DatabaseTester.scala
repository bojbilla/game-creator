package me.reminisce.database

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}
import reactivemongo.api.DefaultDB
import reactivemongo.bson.BSONInteger
import reactivemongo.core.commands.GetLastError

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Awaitable}

abstract class DatabaseTester(actorSystemName: String) extends TestKit(ActorSystem(actorSystemName, ConfigFactory.parseString("akka.loglevel = ERROR")))
with ImplicitSender
with WordSpecLike with BeforeAndAfterAll {

  val attemptsPermitted = 20

  import scala.concurrent.ExecutionContext.Implicits.global

  val safeLastError = new GetLastError(w = Some(BSONInteger(1)))

  var dbs = Set[DefaultDB]()

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    dbs.foreach(_.drop())
  }

  protected def newDb(): DefaultDB = {
    val dbId = DatabaseTestHelper.getDBId
    val connection = DatabaseTestHelper.getConnection
    val db = connection(s"DB$dbId")
    dbs = dbs + db
    db
  }

  def waitAttempts[T](operation: Awaitable[Option[T]], value: Option[T] = None, attempts: Int = 0)
                     (check: T => Boolean): Option[T] = value match {
    case None =>
      if (attempts < attemptsPermitted) {
        Thread.sleep(200)
        val newValue = Await.result(operation, Duration(10, TimeUnit.SECONDS))
        waitAttempts[T](operation, newValue, attempts + 1)(check)
      } else {
        None
      }
    case Some(result) =>
      if (check(result)) {
        Some(result)
      } else {
        Thread.sleep(200)
        val newValue = Await.result(operation, Duration(10, TimeUnit.SECONDS))
        waitAttempts[T](operation, newValue, attempts + 1)(check)
      }
  }
}
