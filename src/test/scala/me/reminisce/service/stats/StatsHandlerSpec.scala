package me.reminisce.service.stats

import java.util.concurrent.TimeUnit

import akka.testkit.TestActorRef
import com.github.nscala_time.time.Imports._
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.fetcher.common.GraphResponses._
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.stats.StatsHandler.{FinalStats, TransientPostsStats}
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class StatsHandlerSpec extends DatabaseTester("OrderByPageLikesSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserStatsHandlerSpec"
  val attemptsPermitted = 15

  "StatsHandler" must {
    "Save \"on the fly\" stats." in {
      val itemsStatsCollection = db[BSONCollection](MongoDatabaseService.itemsStatsCollection)

      val ref = TestActorRef(StatsHandler.props(userId, db))
      ref ! TransientPostsStats(StatsTestData.posts)

      val postIds = StatsTestData.posts.map(p => p.id)
      val selector = BSONDocument("userId" -> userId, "itemId" -> BSONDocument("$in" -> postIds))
      var stats: List[ItemStats] = List()
      var attempts = 0
      while (stats.isEmpty || stats.size < StatsTestData.referenceResult.size) {
        if (attempts > attemptsPermitted) {
          fail("Too many attempts at retrieving stats, maybe not saved.")
        }
        stats = Await.result(itemsStatsCollection.find(selector).cursor[ItemStats].collect[List](postIds.length,
          stopOnError = true), Duration(10, TimeUnit.SECONDS))
        attempts += 1
        Thread.sleep(200)
      }

      assert(stats.nonEmpty)

      assert(stats.size == StatsTestData.referenceResult.size)
      StatsTestData.referenceResult.foreach {
        itemStats => assert(stats.contains(itemStats))
      }
    }
  }

  "Deal with old stats." in {
    val itemsStatsCollection = db[BSONCollection](MongoDatabaseService.itemsStatsCollection)
    val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
    StatsTestData.referenceResult.foreach {
      itemStats =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> itemStats.itemId)
        Await.result(itemsStatsCollection.update(selector, itemStats, upsert = true), Duration(10, TimeUnit.SECONDS))
    }

    val fbPosts = StatsTestData.posts.map(MongoDatabaseService.postToFBPost(_, userId))

    fbPosts.foreach {
      post =>
        val selector = BSONDocument("userId" -> userId, "postId" -> post.postId)
        Await.result(postCollection.update(selector, post, upsert = true), Duration(10, TimeUnit.SECONDS))
    }

    val ref = TestActorRef(StatsHandler.props(userId, db))
    ref ! FinalStats(Set(), Set())

    val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    var userStats: Option[UserStats] = None
    var attempts = 0

    val selector = BSONDocument("userId" -> userId)

    while (userStats.isEmpty) {
      if (attempts > attemptsPermitted) {
        fail("Too many attempts at retrieving stats, maybe not saved.")
      }
      userStats = Await.result(userStatsCollection.find(selector).one[UserStats], Duration(10, TimeUnit.SECONDS))
      attempts += 1
      Thread.sleep(200)
    }

    val expectedUserStats = UserStats(None, "TestUserStatsHandlerSpec",
      Map("LikeNumber" -> 1, "PostWhoCommented" -> 1, "PostGeolocation" -> 1, "Time" -> 1, "PostCommentsNumber" -> 1),
      Map("Order" -> 0, "MultipleChoice" -> 1, "Geolocation" -> 1, "Timeline" -> 1), Set(FBLike("1", "me")))
    
    assert(userStats == Some(expectedUserStats))
  }

}

object StatsTestData {

  val like1 = Like("1", "me")
  val likes1 = Root[List[Like]](data = Some(List(like1)), paging = None, summary = None)
  val pLikes = Post("id1", from = None, message = Some("Message"), story = Some("Story"), place = None, likes = Some(likes1),
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
  val pComs = Post("id2", from = None, message = Some("Message"), story = Some("Story"), place = None, likes = None,
    `type` = None, link = None, created_time = None, attachments = None, comments = Some(com3))

  val l4 = Location(city = None, country = None, latitude = Some(1.0), longitude = Some(1.2), street = None,
    zip = None)
  val pl5 = Place(id = None, name = None, location = Some(l4), created_time = None)
  val pLoc = Post("id3", from = None, message = None, story = None, place = Some(pl5),
    likes = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)

  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
  val now = DateTime.now.toString(formatter)
  val pTime = Post("id4", from = None, message = Some("message"), story = None, place = None, likes = None,
    `type` = None, link = None, created_time = Some(now), attachments = None, comments = None)

  val posts = List(pLikes, pComs, pLoc, pTime)

  val referenceResult =
    List(ItemStats(None, "TestUserStatsHandlerSpec", "id1", "Post", List("LikeNumber"), 1, readForStats = false),
      ItemStats(None, "TestUserStatsHandlerSpec", "id2", "Post", List("PostWhoCommented", "PostCommentsNumber"), 2, readForStats = false),
      ItemStats(None, "TestUserStatsHandlerSpec", "id3", "Post", List("PostGeolocation"), 1, readForStats = false),
      ItemStats(None, "TestUserStatsHandlerSpec", "id4", "Post", List("Time"), 1, readForStats = false))

}