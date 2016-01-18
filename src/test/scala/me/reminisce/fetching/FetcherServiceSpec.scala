package me.reminisce.fetching

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.MongoDBEntities._
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.fetching.FetcherService.FetchData
import me.reminisce.server.domain.Domain.{AlreadyFresh, TooManyRequests}
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class FetcherServiceSpec extends DatabaseTester("FetcherServiceSpec") {


  "FetcherService" must {
    "not fetch when a concurrent fetch has been issued." in {
      val db = newDb()
      val userId = "TestUserFetcherService1"
      val actorRef = TestActorRef(FetcherService.props(db))
      actorRef ! FetchData(userId, "NAN")
      actorRef.suspend()
      val actorRef2 = TestActorRef(FetcherService.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef2, FetchData(userId, "NAN"))
      testProbe.expectMsg(TooManyRequests(s"Already fetching for user $userId"))
    }

    "not fetch when the data is already fresh." in {
      val db = newDb()
      val userId = "TestUserFetcherService2"
      val collection = db[BSONCollection](MongoDatabaseService.lastFetchedCollection)

      val time = DateTime.now

      val update = BSONDocument("userId" -> userId, "date" -> time)

      Await.result(collection.save(update, safeLastError), Duration(10, TimeUnit.SECONDS))
      val testProbe = TestProbe()
      val actorRef = TestActorRef(FetcherService.props(db))
      testProbe.send(actorRef, FetchData(userId, "NAN"))
      testProbe.expectMsg(AlreadyFresh(s"Data for user $userId is fresh."))
    }

  }
}