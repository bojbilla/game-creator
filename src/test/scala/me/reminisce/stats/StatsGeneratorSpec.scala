package me.reminisce.stats

import java.util.concurrent.TimeUnit

import akka.testkit.TestActorRef
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDBEntities._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.database.StatsEntities.{ItemStats, UserStats}
import me.reminisce.fetching.config.GraphResponses._
import me.reminisce.stats.StatsGenerator.{FinalStats, TransientPostsStats}
import me.reminisce.testutils.Retry
import me.reminisce.testutils.database.DatabaseTester
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class StatsGeneratorSpec extends DatabaseTester("StatsGeneratorSpec") {


  "StatsGenerator" must {
    "Save \"on the fly\" stats." in {
      testWithDb {
        db =>
          val itemsStatsCollection = db[BSONCollection](MongoDatabaseService.itemsStatsCollection)

          val ref = TestActorRef(StatsGenerator.props(StatsTestData.userId, db))
          ref ! TransientPostsStats(StatsTestData.posts)

          val postIds = StatsTestData.posts.map(p => p.id)
          val selector = BSONDocument("userId" -> StatsTestData.userId, "itemId" -> BSONDocument("$in" -> postIds))

          Retry.findList[ItemStats](itemsStatsCollection, selector, 0) {
            _.size >= StatsTestData.referenceResult.size
          } match {
            case List() =>
              fail("Failed to retrieve items stats after requesting them.")
            case stats =>
              assert(stats.size == StatsTestData.referenceResult.size)
              StatsTestData.referenceResult.foreach {
                itemStats => assert(stats.contains(itemStats))
              }
          }
      }
    }

    "Deal with old stats." in {
      testWithDb {
        db =>
          val itemsStatsCollection = db[BSONCollection](MongoDatabaseService.itemsStatsCollection)
          val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
          StatsTestData.referenceResult.foreach {
            itemStats =>
              val selector = BSONDocument("userId" -> StatsTestData.userId, "itemId" -> itemStats.itemId)
              Await.result(itemsStatsCollection.update(selector, itemStats, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          val fbPosts = StatsTestData.posts.map(MongoDatabaseService.postToFBPost(_, StatsTestData.userId))

          fbPosts.foreach {
            post =>
              val selector = BSONDocument("userId" -> StatsTestData.userId, "postId" -> post.postId)
              Await.result(postCollection.update(selector, post, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          val ref = TestActorRef(StatsGenerator.props(StatsTestData.userId, db))
          ref ! FinalStats(Set(), Set())

          val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

          val selector = BSONDocument("userId" -> StatsTestData.userId)

          val userStats = Retry.find[UserStats](userStatsCollection, selector, 0)(_ => true)

          val expectedUserStats = UserStats(None, "TestUserStatsHandlerSpec",
            Map("LikeNumber" -> 1, "PostWhoCommented" -> 1, "PostGeolocation" -> 1, "Time" -> 1, "PostCommentsNumber" -> 1),
            Map("Order" -> 0, "MultipleChoice" -> 1, "Geolocation" -> 1, "Timeline" -> 1), Set(FBReaction("1", "me", "")))

          assert(userStats.contains(expectedUserStats))
      }
    }

    "Generate new stats correctly" in {
      testWithDb {
        db =>
          val itemsStatsCollection = db[BSONCollection](MongoDatabaseService.itemsStatsCollection)
          val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
          val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
          val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
          val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

          StatsTestData.referenceResult.foreach {
            itemStats =>
              val selector = BSONDocument("userId" -> StatsTestData.userId, "itemId" -> itemStats.itemId)
              Await.result(itemsStatsCollection.update(selector, itemStats, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          val fbPosts = StatsTestData.posts.map(MongoDatabaseService.postToFBPost(_, StatsTestData.userId))

          fbPosts.foreach {
            post =>
              val selector = BSONDocument("userId" -> StatsTestData.userId, "postId" -> post.postId)
              Await.result(postCollection.update(selector, post, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          StatsTestData.pages.foreach {
            page =>
              val selector = BSONDocument("userId" -> StatsTestData.userId, "pageId" -> page.pageId)
              Await.result(pagesCollection.update(selector, page, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          StatsTestData.pageLikes.foreach {
            pageLike =>
              val selector = BSONDocument("userId" -> StatsTestData.userId, "pageId" -> pageLike.pageId)
              Await.result(pageLikesCollection.update(selector, pageLike, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          val selector = BSONDocument("userId" -> StatsTestData.userId)

          Await.result(userStatsCollection.update(selector, StatsTestData.sampleUserStats, upsert = true), Duration(10, TimeUnit.SECONDS))

          val fbPostIds = fbPosts.map(post => post.postId).toSet
          val fbPageIds = StatsTestData.pages.map(page => page.pageId).toSet

          val ref = TestActorRef(StatsGenerator.props(StatsTestData.userId, db))
          ref ! FinalStats(fbPostIds, fbPageIds)

          //likers is supposed to grow
          val userStats = Retry.find[UserStats](userStatsCollection, selector, 0) {
            _.likers.size > StatsTestData.sampleUserStats.likers.size
          }

          val expectedUserStats = UserStats(None, "TestUserStatsHandlerSpec",
            Map("LikeNumber" -> 22, "PostWhoReacted" -> 1, "PostWhoCommented" -> 2, "PostGeolocation" -> 2, "Time" -> 22,
              "PostCommentsNumber" -> 2), Map("Order" -> 42, "MultipleChoice" -> 3, "Geolocation" -> 2, "Timeline" -> 22),
            Set(FBReaction("1", "me", ""), FBReaction("2", "me2", ""), FBReaction("3", "me3", ""), FBReaction("4", "me4", "")))

          assert(userStats.contains(expectedUserStats))
      }
    }
  }
}

object StatsTestData {

  val userId = "TestUserStatsHandlerSpec"

  val like1 = Reaction("1", "me", "")
  val likes1 = Root[List[Reaction]](data = Some(List(like1)), paging = None, summary = None)
  val pLikes = Post("id1", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = Some(likes1),
    `type` = None, link = None, created_time = None, attachments = None, comments = None)

  val from2 = From(id = "2", name = "a")
  val comment2 = Comment(id = "", from = from2, like_count = 0, message = "", attachments = None)
  val from3 = From(id = "3", name = "s")
  val comment3 = Comment(id = "3", from = from3, like_count = 0, message = "", attachments = None)
  val from4 = From(id = "4", name = "d")
  val comment4 = Comment(id = "4", from = from4, like_count = 0, message = "", attachments = None)
  val from5 = From(id = "5", name = "f")
  val comment5 = Comment(id = "5", from = from5, like_count = 0, message = "", attachments = None)
  val com3 = Root[List[Comment]](data = Option(List(comment2, comment3, comment4, comment5)), paging = None, summary = None)
  val pComments = Post("id2", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = None,
    `type` = None, link = None, created_time = None, attachments = None, comments = Some(com3))

  val l4 = Location(city = None, country = None, latitude = Some(1.0), longitude = Some(1.2), street = None,
    zip = None)
  val pl5 = Place(id = None, name = None, location = Some(l4), created_time = None)
  val pLoc = Post("id3", from = None, message = None, story = None, place = Some(pl5),
    reactions = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)

  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
  val now = DateTime.now
  val nowString = now.toString(formatter)
  val pTime = Post("id4", from = None, message = Some("message"), story = None, place = None, reactions = None,
    `type` = None, link = None, created_time = Some(nowString), attachments = None, comments = None)

  val posts = List(pLikes, pComments, pLoc, pTime)

  val referenceResult =
    List(ItemStats(None, "TestUserStatsHandlerSpec", "id1", "Post", List("LikeNumber"), 1, readForStats = false),
      ItemStats(None, "TestUserStatsHandlerSpec", "id2", "Post", List("PostWhoCommented", "PostCommentsNumber"), 2, readForStats = false),
      ItemStats(None, "TestUserStatsHandlerSpec", "id3", "Post", List("PostGeolocation"), 1, readForStats = false),
      ItemStats(None, "TestUserStatsHandlerSpec", "id4", "Post", List("Time"), 1, readForStats = false))

  val pages = (1 to 10).map {
    index =>
      FBPage(None, s"${index}I$index", Some(s"Page$index"), None, index * index * 7)
  }

  val pageLikes = (1 to 5).map {
    index =>
      FBPageLike(None, s"$userId", pages(index).pageId, now - index.months)
  }

  val sampleUserStats = UserStats(None, userId,
    Map("LikeNumber" -> 11, "PostWhoCommented" -> 1, "PostGeolocation" -> 1, "Time" -> 11, "PostCommentsNumber" -> 1),
    Map("Order" -> 18, "MultipleChoice" -> 1, "Geolocation" -> 1, "Timeline" -> 11),
    Set(FBReaction("2", "me2", ""), FBReaction("3", "me3", ""), FBReaction("4", "me4", "")))

}