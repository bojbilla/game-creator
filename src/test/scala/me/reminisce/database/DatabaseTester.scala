package me.reminisce.database

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, WordSpecLike}
import reactivemongo.core.commands.GetLastError

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Awaitable}

abstract class DatabaseTester(actorSystemName: String) extends TestKit(ActorSystem(actorSystemName, ConfigFactory.parseString("akka.loglevel = ERROR")))
with ImplicitSender
with WordSpecLike with BeforeAndAfterAll with BeforeAndAfterEach {

  val attemptsPermitted = 20

  import scala.concurrent.ExecutionContext.Implicits.global

  val db = DatabaseTestHelper.getDb
  val safeLastError = new GetLastError(fsync = true)

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
  }

  override def afterEach(): Unit = {
    db.drop()
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
