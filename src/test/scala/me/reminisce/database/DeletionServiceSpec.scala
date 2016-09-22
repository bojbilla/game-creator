package me.reminisce.database

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.DeletionService.{ClearDatabase, RemoveExtraLikes, RemoveUser}
import me.reminisce.database.MongoDBEntities._
import me.reminisce.server.ApplicationConfiguration
import me.reminisce.server.domain.Domain.{ActionForbidden, Done}
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class DeletionServiceSpec extends DatabaseTester("DeletionServiceSpec") {


  "DeletionService" must {
    "delete user without error." in {
      val db = newDb()
      val collection = db[BSONCollection](MongoDatabaseService.lastFetchedCollection)

      val userId = "TestUser"
      val time = DateTime.now

      val update = BSONDocument("userId" -> userId, "date" -> time)

      Await.result(collection.update(update, update, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))
      val actorRef = TestActorRef(DeletionService.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, RemoveUser(userId))
      testProbe.expectMsg(Done("Deletion performed without error."))
    }

    "delete extra likes without error." in {
      val db = newDb()
      val userId = "TestUserDeletionService"
      val likes = Set("likedThis", "andThis")
      val actorRef = TestActorRef(DeletionService.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, RemoveExtraLikes(userId, likes))
      testProbe.expectMsg(Done("Deletion performed without error."))
    }

    "clear database only in dev mode." in {
      val db = newDb()

      //sample data to be deleted
      val collection = db[BSONCollection](MongoDatabaseService.lastFetchedCollection)

      val userId = "TestUser"
      val time = DateTime.now

      val update = BSONDocument("userId" -> userId, "date" -> time)

      Await.result(collection.update(update, update, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))
      val actorRef = TestActorRef(DeletionService.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, ClearDatabase())
      if (ApplicationConfiguration.appMode == "DEV") {
        testProbe.expectMsg(Done("Deletion performed without error."))
      } else {
        testProbe.expectMsg(ActionForbidden("The app is not in development mode."))
      }
    }
  }
}