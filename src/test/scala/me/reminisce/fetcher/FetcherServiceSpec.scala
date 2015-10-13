package me.reminisce.fetcher

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.{DatabaseTestHelper, MongoDatabaseService}
import me.reminisce.fetcher.FetcherService.FetchData
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.server.domain.Domain.{AlreadyFresh, TooManyRequests}
import org.joda.time.DateTime
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class FetcherServiceSpec extends TestKit(ActorSystem("FetcherServiceSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  val port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")

  val userId = "TestUserFetcherService"


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

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