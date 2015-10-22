package me.reminisce.fetcher

import java.util.concurrent.TimeUnit

import akka.testkit.TestActorRef
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
class FetcherServiceSpec extends DatabaseTester {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserFetcherService"

  "FetcherService" must {
    "not fetch when a concurrent fetch has been issued." in {
      val actorRef = TestActorRef(FetcherService.props(db))
      actorRef ! FetchData(userId, "NAN")
      actorRef.suspend()
      val actorRef2 = TestActorRef(FetcherService.props(db))
      actorRef2 ! FetchData(userId, "NAN")
      expectMsg(TooManyRequests(s"Already fetching for user $userId"))
      FetcherService.currentlyFetching = Set()
    }

    "not fetch when the data is already fresh." in {
      val collection = db[BSONCollection](MongoDatabaseService.lastFetchedCollection)

      val time = DateTime.now
      val selector = BSONDocument("userId" -> userId)

      val update = BSONDocument("userId" -> userId, "date" -> time)

      Await.result(collection.update(selector, update, upsert = true), Duration(10, TimeUnit.SECONDS))
      val actorRef = TestActorRef(new FetcherService(db))
      actorRef ! FetchData(userId, "NAN")
      expectMsg(AlreadyFresh(s"Data for user $userId is fresh."))
    }

  }
}