package me.reminisce.service.stats

import com.github.nscala_time.time.Imports._
import me.reminisce.fetcher.common.GraphResponses._
import me.reminisce.mongodb.MongoDBEntities.FBComment
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
    val incType = map.toList.head._1
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
    assert(timeIsHere.head == StatsDataTypes.Time)

    val messageAndTime = Post("id", from = None, message = Some("message"), story = None, place = None, likes = None,
      `type` = None, link = None, created_time = Some(now), attachments = None, comments = None)
    val timeIsAlsoHere = StatsHandler.availableDataTypes(messageAndTime)
    assert(timeIsAlsoHere.nonEmpty && timeIsAlsoHere.size == 1)
    assert(timeIsAlsoHere.head == StatsDataTypes.Time)
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
    assert(dataTypes.head == StatsDataTypes.PostGeolocation)
  }

  test("WhoCommented and CommentsNumber data types."){
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
}
