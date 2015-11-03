package me.reminisce.fetcher

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.fetcher.FetcherService.FetchData
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.server.domain.Domain.{AlreadyFresh, TooManyRequests}
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class FetcherServiceSpec extends DatabaseTester("FetcherServiceSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  "FetcherService" must {
    "not fetch when a concurrent fetch has been issued." in {
      val userId = "TestUserFetcherService1"
      val actorRef = TestActorRef(FetcherService.props(db))
      actorRef ! FetchData(userId, "NAN")
      actorRef.suspend()
      val actorRef2 = TestActorRef(FetcherService.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef2, FetchData(userId, "NAN"))
      testProbe.expectMsg(TooManyRequests(s"Already fetching for user $userId"))
      FetcherService.currentlyFetching = Set()
    }

    "not fetch when the data is already fresh." in {
      val userId = "TestUserFetcherService2"
      val collection = db[BSONCollection](MongoDatabaseService.lastFetchedCollection)

      val time = DateTime.now
      val selector = BSONDocument("userId" -> userId)

      val update = BSONDocument("userId" -> userId, "date" -> time)

      Await.result(collection.update(selector, update, upsert = true), Duration(10, TimeUnit.SECONDS))
      val testInsert = waitAttempts[LastFetched](collection.find(selector).one[LastFetched])(_.date == time)
      testInsert match {
        case Some(lastFetched) =>
          val testProbe = TestProbe()
          val actorRef = TestActorRef(FetcherService.props(db))
          testProbe.send(actorRef, FetchData(userId, "NAN"))
          testProbe.expectMsg(AlreadyFresh(s"Data for user $userId is fresh."))
        case None =>
          fail("Insertion of lastfeched failed.")
      }
    }

  }
}