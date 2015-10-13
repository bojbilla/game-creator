package me.reminisce.database

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.MongoDatabaseService.{SaveFBPage, SaveFBPost, SaveLastFetchedTime}
import me.reminisce.mongodb.MongoDBEntities._
import org.joda.time.DateTime
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class MongoDatabaseServiceSpec extends TestKit(ActorSystem("DeletionServiceSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  val port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val attemptsPermitted = 10
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

  "MongoDatabaseService" must {
    "save post to database." in {

      val userId = PostTestsData.userId
      val post = PostTestsData.post

      val dbService = TestActorRef(new MongoDatabaseService(userId, db))

      dbService ! SaveFBPost(List(post))

      val collection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val postId = PostTestsData.postId

      val selector = BSONDocument("userId" -> userId, "postId" -> postId)

      var fbPost: Option[FBPost] = None
      var attempts = 0
      while (fbPost.isEmpty) {
        if (attempts > attemptsPermitted) {
          fail("Too many attempts at retrieving post, maybe not saved.")
        }
        fbPost = Await.result(collection.find(selector).one[FBPost], Duration(10, TimeUnit.SECONDS))
        attempts += 1
        Thread.sleep(200)
      }
      assert(fbPost.get.postId == postId)
    }

    "save page to database." in {
      val userId = PageTestsData.userId
      val page = PageTestsData.page

      val dbService = TestActorRef(new MongoDatabaseService(userId, db))

      dbService ! SaveFBPage(List(page))

      val collectionPages = db[BSONCollection](MongoDatabaseService.fbPagesCollection)

      val pageId = PageTestsData.pageId

      val selectorPage = BSONDocument("pageId" -> pageId)

      var attempts = 0
      var fbPage: Option[FBPage] = None
      while (fbPage.isEmpty) {
        if (attempts > attemptsPermitted) {
          fail("Too many attempts at retrieving page, maybe not saved.")
        }
        fbPage = Await.result(collectionPages.find(selectorPage).one[FBPage], Duration(10, TimeUnit.SECONDS))
        attempts += 1
        Thread.sleep(200)
      }
      assert(fbPage.get.pageId == pageId)

      val collectionPageLikes = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)

      val selectorLikes = BSONDocument("userId" -> userId, "pageId" -> pageId)

      attempts = 0
      var fBPageLike: Option[FBPageLike] = None
      while (fBPageLike.isEmpty) {
        if (attempts > attemptsPermitted) {
          fail("Too many attempts at retrieving page like, maybe not saved.")
        }
        fBPageLike = Await.result(collectionPageLikes.find(selectorLikes).one[FBPageLike], Duration(10, TimeUnit.SECONDS))
        attempts += 1
        Thread.sleep(200)
      }

      assert(fBPageLike.get.pageId == pageId)
      assert(fBPageLike.get.userId == userId)
    }

    "save last fetched time to database." in {
      val now = DateTime.now

      val userId = "UserId"

      val dbService = TestActorRef(new MongoDatabaseService(userId, db))

      dbService ! SaveLastFetchedTime

      val collection = db[BSONCollection](MongoDatabaseService.lastFetchedCollection)

      val selector = BSONDocument("userId" -> userId)

      var fbLastFetched: Option[LastFetched] = None
      var attempts = 0
      while (fbLastFetched.isEmpty) {
        if (attempts > attemptsPermitted) {
          fail(s"Too many attempts ($attempts) at retrieving last fetched time, maybe not saved.")
        }
        fbLastFetched = Await.result(collection.find(selector).one[LastFetched], Duration(10, TimeUnit.SECONDS))
        attempts += 1
        Thread.sleep(200)
      }
      assert(fbLastFetched.get.userId == userId)
      assert(fbLastFetched.get.date.isAfter(now.getMillis))
    }
  }
}