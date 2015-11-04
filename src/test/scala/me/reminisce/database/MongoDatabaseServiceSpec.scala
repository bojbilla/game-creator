package me.reminisce.database

import akka.testkit.TestActorRef
import me.reminisce.database.MongoDatabaseService.{SaveFBPage, SaveFBPost, SaveLastFetchedTime}
import me.reminisce.mongodb.MongoDBEntities._
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

@DoNotDiscover
class MongoDatabaseServiceSpec extends DatabaseTester("MongoDatabaseServiceSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  "MongoDatabaseService" must {
    "save post to database." in {
      val db = newDb()
      val userId = PostTestsData.userId
      val post = PostTestsData.post

      val dbService = TestActorRef(new MongoDatabaseService(userId, db))

      dbService ! SaveFBPost(List(post))

      val collection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val postId = PostTestsData.postId

      val selector = BSONDocument("userId" -> userId, "postId" -> postId)

      waitAttempts[FBPost](collection.find(selector).one[FBPost])(_ => true) match {
        case Some(fbPost) => assert(fbPost.postId == postId)
        case None =>
          fail("Too many attempts at retrieving post, maybe not saved.")
      }
      db.drop()
    }

    "save page to database." in {
      val db = newDb()
      val userId = PageTestsData.userId
      val page = PageTestsData.page

      val dbService = TestActorRef(new MongoDatabaseService(userId, db))

      dbService ! SaveFBPage(List(page))

      val collectionPages = db[BSONCollection](MongoDatabaseService.fbPagesCollection)

      val pageId = PageTestsData.pageId

      val selectorPage = BSONDocument("pageId" -> pageId)

      waitAttempts[FBPage](collectionPages.find(selectorPage).one[FBPage])(_ => true) match {
        case Some(fbPage) =>
          assert(fbPage.pageId == pageId)
        case None =>
          fail("Too many attempts at retrieving page, maybe not saved.")
      }

      val collectionPageLikes = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)

      val selectorLikes = BSONDocument("userId" -> userId, "pageId" -> pageId)

      waitAttempts[FBPageLike](collectionPageLikes.find(selectorLikes).one[FBPageLike])(_ => true) match {
        case Some(fBPageLike) =>
          assert(fBPageLike.pageId == pageId)
          assert(fBPageLike.userId == userId)
        case None =>
          fail("Too many attempts at retrieving page like, maybe not saved.")
      }
      db.drop()
    }

    "save last fetched time to database." in {
      val db = newDb()
      val now = DateTime.now

      val userId = "UserId"

      val dbService = TestActorRef(new MongoDatabaseService(userId, db))

      dbService ! SaveLastFetchedTime

      val collection = db[BSONCollection](MongoDatabaseService.lastFetchedCollection)

      val selector = BSONDocument("userId" -> userId)

      waitAttempts[LastFetched](collection.find(selector).one[LastFetched])(_ => true) match {
        case Some(fbLastFetched) =>
          assert(fbLastFetched.userId == userId)
          assert(fbLastFetched.date.isAfter(now.getMillis))
        case None =>
          fail(s"Too many attempts (${attemptsPermitted + 1}) at retrieving last fetched time, maybe not saved.")
      }
      db.drop()
    }
  }
}