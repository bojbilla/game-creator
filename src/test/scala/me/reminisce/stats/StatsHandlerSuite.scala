package me.reminisce.stats

import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDBEntities.FBLike
import me.reminisce.database.StatsEntities.{ItemStats, UserStats}
import me.reminisce.fetching.config.GraphResponses._
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.questions.QuestionGenerationConfig
import me.reminisce.stats.StatsDataTypes.{PostCommentsNumber, PostGeolocation, PostWhoCommented, PostWhoLiked}
import me.reminisce.testutils.AssertHelpers._
import org.joda.time.DateTime
import org.scalatest.FunSuite

class StatsHandlerSuite extends FunSuite {

  test("Adding a new question type to a map of type -> count.") {
    val map = Map[String, Int](("T1", 2), ("T2", 4), ("T3", 11))
    val noExtension = StatsHandler.addTypesToMap(List(), map)
    assert(noExtension.size == map.size)
    map.foreach {
      case (k, v) =>
        assert(noExtension.contains(k))
        assert(noExtension.getOrElse(k, 0) == v)
    }

    val newType = "T4"
    val newTypeCount = 3
    val incType = map.toList.headOption match {
      case Some((tpe, count)) => tpe
      case None => fail("Something went terribly wrong.")
    }
    val incTypeCount = 2


    val extended = StatsHandler.addTypesToMap(List[(String, Int)]((newType, newTypeCount), (incType, incTypeCount)), map)

    assert(extended.size == map.size + 1)
    map.foreach {
      case (k, v) =>
        assert(extended.contains(k))
        val expectedCount = if (k == incType) v + incTypeCount else v
        assert(extended.getOrElse(k, 0) == expectedCount)
    }

    assert(extended.contains(newType))
    assert(extended.getOrElse(newType, 0) == newTypeCount)
  }

  test("Empty post data types.") {
    val emptyPost = Post("id", from = None, message = None, story = None, place = None, likes = None, `type` = None,
      link = None, created_time = None, attachments = None, comments = None)
    val emptyTypes = StatsHandler.availableDataTypes(emptyPost)
    assert(emptyTypes.isEmpty)
  }

  test("Time data type.") {
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
    val now = DateTime.now.toString(formatter)
    val storyAndTime = Post("id", from = None, message = None, story = Some("story"), place = None, likes = None,
      `type` = None, link = None, created_time = Some(now), attachments = None, comments = None)
    val timeIsHere = StatsHandler.availableDataTypes(storyAndTime)
    assert(timeIsHere.nonEmpty && timeIsHere.size == 1)
    listHeadAssert(timeIsHere)(tpe => tpe == StatsDataTypes.Time)(() => fail("Type not defined."))

    val messageAndTime = Post("id", from = None, message = Some("message"), story = None, place = None, likes = None,
      `type` = None, link = None, created_time = Some(now), attachments = None, comments = None)
    val timeIsAlsoHere = StatsHandler.availableDataTypes(messageAndTime)
    assert(timeIsAlsoHere.nonEmpty && timeIsAlsoHere.size == 1)
    listHeadAssert(timeIsAlsoHere)(tpe => tpe == StatsDataTypes.Time)(() => fail("Type not defined."))
  }

  test("Geolocation data type.") {
    val pl1 = Place(id = None, name = None, location = None, created_time = None)
    val p1 = Post("id", from = None, message = None, story = None, place = Some(pl1),
      likes = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(StatsHandler.availableDataTypes(p1).isEmpty)

    val l1 = Location(city = None, country = None, latitude = None, longitude = None, street = None,
      zip = None)
    val pl2 = Place(id = None, name = None, location = Some(l1), created_time = None)
    val p2 = Post("id", from = None, message = None, story = None, place = Some(pl2),
      likes = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(StatsHandler.availableDataTypes(p2).isEmpty)

    val l2 = Location(city = None, country = None, latitude = Some(1.0), longitude = None, street = None,
      zip = None)
    val pl3 = Place(id = None, name = None, location = Some(l2), created_time = None)
    val p3 = Post("id", from = None, message = None, story = None, place = Some(pl3),
      likes = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(StatsHandler.availableDataTypes(p3).isEmpty)

    val l3 = Location(city = None, country = None, latitude = None, longitude = Some(1.2), street = None,
      zip = None)
    val pl4 = Place(id = None, name = None, location = Some(l3), created_time = None)
    val p4 = Post("id", from = None, message = None, story = None, place = Some(pl4),
      likes = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(StatsHandler.availableDataTypes(p4).isEmpty)

    val l4 = Location(city = None, country = None, latitude = Some(1.0), longitude = Some(1.2), street = None,
      zip = None)
    val pl5 = Place(id = None, name = None, location = Some(l4), created_time = None)
    val p5 = Post("id", from = None, message = None, story = None, place = Some(pl5),
      likes = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    val dataTypes = StatsHandler.availableDataTypes(p5)
    assert(dataTypes.nonEmpty && dataTypes.size == 1)
    listHeadAssert(dataTypes)(tpe => tpe == StatsDataTypes.PostGeolocation)(() => fail("Type not defined."))
  }

  test("WhoCommented and CommentsNumber data types.") {
    val p1 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, likes = None,
      `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(!StatsHandler.availableDataTypes(p1).contains(StatsDataTypes.PostWhoCommented))
    assert(!StatsHandler.availableDataTypes(p1).contains(StatsDataTypes.PostCommentsNumber))

    val from2 = From(id = "", name = "")
    val comment2 = Comment(id = "", from = from2, like_count = 0, message = "", attachments = None)
    val com2 = Root[List[Comment]](data = Option(List(comment2)), paging = None, summary = None)
    val p2 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, likes = None,
      `type` = None, link = None, created_time = None, attachments = None, comments = Some(com2))
    assert(!StatsHandler.availableDataTypes(p2).contains(StatsDataTypes.PostWhoCommented))
    assert(!StatsHandler.availableDataTypes(p2).contains(StatsDataTypes.PostCommentsNumber))

    val from3 = From(id = "3", name = "")
    val comment3 = Comment(id = "3", from = from3, like_count = 0, message = "", attachments = None)
    val from4 = From(id = "4", name = "")
    val comment4 = Comment(id = "4", from = from4, like_count = 0, message = "", attachments = None)
    val from5 = From(id = "5", name = "")
    val comment5 = Comment(id = "5", from = from5, like_count = 0, message = "", attachments = None)
    val com3 = Root[List[Comment]](data = Option(List(comment2, comment3, comment4, comment5)), paging = None, summary = None)
    val p3 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, likes = None,
      `type` = None, link = None, created_time = None, attachments = None, comments = Some(com3))
    assert(StatsHandler.availableDataTypes(p3).contains(StatsDataTypes.PostWhoCommented))
    assert(StatsHandler.availableDataTypes(p3).contains(StatsDataTypes.PostCommentsNumber))
  }

  test("LikesNumber data type.") {
    val p1 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, likes = None,
      `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(!StatsHandler.availableDataTypes(p1).contains(StatsDataTypes.LikeNumber))

    val like1 = Like("1", "")
    val likes1 = Root[List[Like]](data = Some(List(like1)), paging = None, summary = None)
    val p2 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, likes = Some(likes1),
      `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(StatsHandler.availableDataTypes(p2).contains(StatsDataTypes.LikeNumber))
  }

  test("Extract item stats from list of items stats.") {
    val itemsNumber = 10
    val users = (0 until itemsNumber).map {
      nb => s"User$nb"
    }
    val items = (0 until itemsNumber).map {
      nb => s"Item$nb"
    }
    val types = (0 until itemsNumber).map {
      nb => s"Type$nb"
    }
    val dataTypes = (0 until itemsNumber).map {
      nb => (0 to nb).map(nbb => s"dType$nbb").toList // no empty list here (usage of to)
    }
    val counts = 1 to itemsNumber //No count equal to 0
    val stats = (0 until itemsNumber).map {
        nb => ItemStats(None, users(nb), items(nb), types(nb), dataTypes(nb), counts(nb), readForStats = true)
      }.toList

    val empty = StatsHandler.getItemStats("Ha", "He", "Hi", stats)
    assert(empty.userId == "Ha")
    assert(empty.itemId == "He")
    assert(empty.itemType == "Hi")
    assert(empty.dataTypes.isEmpty)
    assert(empty.dataCount == 0)

    val findThis = itemsNumber / 2
    val foundIt = StatsHandler.getItemStats(users(findThis), items(findThis), types(findThis), stats)
    assert(foundIt.userId == users(findThis))
    assert(foundIt.itemId == items(findThis))
    assert(foundIt.itemType == types(findThis))
    assert(foundIt.dataTypes == dataTypes(findThis))
    assert(foundIt.dataCount == counts(findThis))
  }

  test("Add new counts to user stats.") {
    val userId = "UserId"
    val oldLikers = (1 to 10).map(nb => FBLike(s"user$nb", s"name$nb")).toSet
    val newLikers = (11 to 20).map(nb => FBLike(s"user$nb", s"name$nb")).toSet
    assert(oldLikers != newLikers)

    val oldDataTypes = Map((PostGeolocation.name, 2), (PostWhoCommented.name, 4), (PostWhoLiked.name, 13)) // linked with below counts
    val newDataTypes = List((PostWhoLiked.name, 7), (PostCommentsNumber.name, 17)) // prime number is important to test the rounding
    val newItemsStats = newDataTypes.flatMap {
        case (tpe, count) => (1 to count).map {
          any => ItemStats(userId = userId, itemId = s"item$userId", itemType = "Post", dataTypes = List(tpe), dataCount = 1)
        }
      }

    val expectedOrderCount = 17 - (17 % QuestionGenerationConfig.orderingItemsNumber)

    val oldQuestionCounts = Map((Geolocation.toString, 2), (MultipleChoice.toString, 17))

    val userStats = UserStats(userId = userId, dataTypeCounts = oldDataTypes, questionCounts = oldQuestionCounts,
      likers = oldLikers)

    val newUserStats = StatsHandler.userStatsWithNewCounts(newLikers, newItemsStats, userStats)

    assert(userStats.userId == newUserStats.userId)
    assert(newUserStats.likers == newLikers)

    assert(newUserStats.dataTypeCounts.size == userStats.dataTypeCounts.size + 1)
    assert(newUserStats.dataTypeCounts.getOrElse(PostGeolocation.name, 0) == 2)
    assert(newUserStats.dataTypeCounts.getOrElse(PostWhoCommented.name, 0) == 4)
    assert(newUserStats.dataTypeCounts.getOrElse(PostWhoLiked.name, 0) == 20)
    assert(newUserStats.dataTypeCounts.getOrElse(PostCommentsNumber.name, 0) == 17)

    assert(newUserStats.questionCounts.size == userStats.questionCounts.size + 1)
    assert(newUserStats.questionCounts.getOrElse(Geolocation.toString, 0) == 2)
    assert(newUserStats.questionCounts.getOrElse(MultipleChoice.toString, 0) == 24)
    assert(newUserStats.questionCounts.getOrElse(Order.toString, 0) == expectedOrderCount)
  }

}
