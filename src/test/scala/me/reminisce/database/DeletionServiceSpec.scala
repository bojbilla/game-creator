package me.reminisce.database

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.DeletionService.{ClearDatabase, RemoveExtraLikes, RemoveUser}
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.server.domain.Domain.{ActionForbidden, Done}
import me.reminisce.service.ApplicationConfiguration
import org.joda.time.DateTime
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class DeletionServiceSpec extends TestKit(ActorSystem("DeletionServiceSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  val port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

  "DeletionService" must {
    "delete user without error." in {
      val collection = db[BSONCollection](MongoDatabaseService.lastFetchedCollection)

      val userId = "TestUser"
      val time = DateTime.now
      val selector = BSONDocument("userId" -> userId)

      val update = BSONDocument("userId" -> userId, "date" -> time)

      Await.result(collection.update(selector, update, upsert = true), Duration(10, TimeUnit.SECONDS))
      val actorRef = TestActorRef(new DeletionService(db))
      actorRef ! RemoveUser(userId)
      expectMsg(Done("Deletion performed without error."))
    }

    "delete extra likes without error." in {
      val userId = "TestUserDeletionService"
      val likes = Set("likedThis", "andThis")
      val actorRef = TestActorRef(new DeletionService(db))
      actorRef ! RemoveExtraLikes(userId, likes)
      expectMsg(Done("Deletion performed without error."))
    }

    "clear database only in dev mode." in {
      val actorRef = TestActorRef(new DeletionService(db))
      actorRef ! ClearDatabase()
      if (ApplicationConfiguration.appMode == "DEV") {
        expectMsg(Done("Deletion performed without error."))
      } else {
        expectMsg(ActionForbidden("The app is not in development mode."))
      }
    }
  }
}