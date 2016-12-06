package me.reminisce.analysis

import com.github.nscala_time.time.Imports._
import me.reminisce.analysis.DataTypes._
import me.reminisce.database.AnalysisEntities.{ItemSummary, UserSummary}
import me.reminisce.database.MongoDBEntities.{AbstractReaction, FBFrom, FBReaction}
import me.reminisce.fetching.config.GraphResponses._
import me.reminisce.gameboard.board.GameboardEntities.{Geolocation, MultipleChoice, Order, QuestionKind}
import me.reminisce.gameboard.questions.QuestionGenerationConfig
import org.joda.time.DateTime
import org.scalatest.FunSuite

import scala.util.Random

class DataAnalyserSuite extends FunSuite {

  test("Adding a new question type to a map of type -> count.") {
    val map = Map[String, Int](("T1", 2), ("T2", 4), ("T3", 11))
    val noExtension = DataAnalyser.addTypesToMap(List(), map)
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


    val extended = DataAnalyser.addTypesToMap(List[(String, Int)]((newType, newTypeCount), (incType, incTypeCount)), map)

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
    val emptyPost = Post("id", from = None, message = None, story = None, place = None, reactions = None, `type` = None,
      link = None, created_time = None, attachments = None, comments = None)
    val emptyTypes = DataAnalyser.availableDataTypes(emptyPost)
    assert(emptyTypes.isEmpty)
  }

  test("Time data type.") {
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
    val now = DateTime.now.toString(formatter)
    val storyAndTime = Post("id", from = None, message = None, story = Some("story"), place = None, reactions = None,
      `type` = None, link = None, created_time = Some(now), attachments = None, comments = None)
    val timeIsHere = DataAnalyser.availableDataTypes(storyAndTime)
    assert(timeIsHere.nonEmpty && timeIsHere.size == 1)
    assert(timeIsHere.contains(Time))

    val messageAndTime = Post("id", from = None, message = Some("message"), story = None, place = None, reactions = None,
      `type` = None, link = None, created_time = Some(now), attachments = None, comments = None)
    val timeIsAlsoHere = DataAnalyser.availableDataTypes(messageAndTime)
    assert(timeIsAlsoHere.nonEmpty && timeIsAlsoHere.size == 1)
    assert(timeIsAlsoHere.contains(Time))
  }

  test("Geolocation data type.") {
    val pl1 = Place(id = None, name = None, location = None, created_time = None)
    val p1 = Post("id", from = None, message = None, story = None, place = Some(pl1),
      reactions = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(DataAnalyser.availableDataTypes(p1).isEmpty)

    val l1 = Location(city = None, country = None, latitude = None, longitude = None, street = None,
      zip = None)
    val pl2 = Place(id = None, name = None, location = Some(l1), created_time = None)
    val p2 = Post("id", from = None, message = None, story = None, place = Some(pl2),
      reactions = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(DataAnalyser.availableDataTypes(p2).isEmpty)

    val l2 = Location(city = None, country = None, latitude = Some(1.0), longitude = None, street = None,
      zip = None)
    val pl3 = Place(id = None, name = None, location = Some(l2), created_time = None)
    val p3 = Post("id", from = None, message = None, story = None, place = Some(pl3),
      reactions = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(DataAnalyser.availableDataTypes(p3).isEmpty)

    val l3 = Location(city = None, country = None, latitude = None, longitude = Some(1.2), street = None,
      zip = None)
    val pl4 = Place(id = None, name = None, location = Some(l3), created_time = None)
    val p4 = Post("id", from = None, message = None, story = None, place = Some(pl4),
      reactions = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(DataAnalyser.availableDataTypes(p4).isEmpty)

    val l4 = Location(city = None, country = None, latitude = Some(1.0), longitude = Some(1.2), street = None,
      zip = None)
    val pl5 = Place(id = None, name = None, location = Some(l4), created_time = None)
    val p5 = Post("id", from = None, message = None, story = None, place = Some(pl5),
      reactions = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)
    val dataTypes = DataAnalyser.availableDataTypes(p5)
    assert(dataTypes.nonEmpty && dataTypes.size == 1)
    assert(dataTypes.contains(PostGeolocation))
  }

  test("CommentsNumber data types.") {
    val p1 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = None,
      `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(!DataAnalyser.availableDataTypes(p1).contains(DataTypes.PostWhoCommented))
    assert(!DataAnalyser.availableDataTypes(p1).contains(DataTypes.PostCommentsNumber))

    val from2 = From(id = "", name = "")
    val comment2 = Comment(id = "", from = from2, like_count = 0, message = "", attachments = None)
    val com2 = Root[List[Comment]](data = Option(List(comment2)), paging = None, summary = None)
    val p2 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = None,
      `type` = None, link = None, created_time = None, attachments = None, comments = Some(com2))
    assert(!DataAnalyser.availableDataTypes(p2).contains(DataTypes.PostWhoCommented))
    assert(!DataAnalyser.availableDataTypes(p2).contains(DataTypes.PostCommentsNumber))

    val from3 = From(id = "3", name = "")
    val comment3 = Comment(id = "3", from = from3, like_count = 0, message = "", attachments = None)
    val from4 = From(id = "4", name = "")
    val comment4 = Comment(id = "4", from = from4, like_count = 0, message = "", attachments = None)
    val from5 = From(id = "5", name = "")
    val comment5 = Comment(id = "5", from = from5, like_count = 0, message = "", attachments = None)
    val com3 = Root[List[Comment]](data = Option(List(comment2, comment3, comment4, comment5)), paging = None, summary = None)
    val p3 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = None,
      `type` = None, link = None, created_time = None, attachments = None, comments = Some(com3))
    assert(DataAnalyser.availableDataTypes(p3).contains(DataTypes.PostCommentsNumber))
  }

  test("LikesNumber data type.") {
    val p1 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = None,
      `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(!DataAnalyser.availableDataTypes(p1).contains(DataTypes.PostReactionNumber))

    val like1 = Reaction("1", "", "")
    val likes1 = Root[List[Reaction]](data = Some(List(like1)), paging = None, summary = None)
    val p2 = Post("id", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = Some(likes1),
      `type` = None, link = None, created_time = None, attachments = None, comments = None)
    assert(DataAnalyser.availableDataTypes(p2).contains(DataTypes.PostReactionNumber))
  }

  test("Extract item summary from list of items summaries.") {
    val itemsNumber = 10

    val possibleItemTypes: List[ItemType] = Random.shuffle(List(PostType, PageType))

    val users = (0 until itemsNumber).map {
      nb => s"User$nb"
    }
    val items = (0 until itemsNumber).map {
      nb => s"Item$nb"
    }
    val types = (0 until itemsNumber).map {
      nb => possibleItemTypes(nb % possibleItemTypes.length)
    }

    val possibleDataTypes: List[DataType] = Random.shuffle(List(Time, PostCommentsNumber, PostGeolocation, PostWhoCommented, PostWhoReacted))

    val dataTypes = (0 until itemsNumber).map {
      nb => (0 to nb).map(nbb => possibleDataTypes(nb % possibleDataTypes.length)).toSet // no empty list here (usage of to)
    }
    val counts = 1 to itemsNumber //No count equal to 0
    val itemSummaries = (0 until itemsNumber).map {
      nb => ItemSummary(None, users(nb), items(nb), types(nb), dataTypes(nb), counts(nb))
    }.toList

    val empty = DataAnalyser.getItemSummary("Ha", "He", PostType, itemSummaries)
    assert(empty.userId == "Ha")
    assert(empty.itemId == "He")
    assert(empty.itemType == PostType)
    assert(empty.dataTypes.isEmpty)
    assert(empty.dataCount == 0)

    val findThis = itemsNumber / 2
    val foundIt = DataAnalyser.getItemSummary(users(findThis), items(findThis), types(findThis), itemSummaries)
    assert(foundIt.userId == users(findThis))
    assert(foundIt.itemId == items(findThis))
    assert(foundIt.itemType == types(findThis))
    assert(foundIt.dataTypes == dataTypes(findThis))
    assert(foundIt.dataCount == counts(findThis))
  }

  test("Add new counts to user summary.") {
    val userId = "UserId"
    val oldReactioners: Set[AbstractReaction] = (1 to 10).map(nb => FBReaction(FBFrom(s"user$nb", s"name$nb"), PostWhoLiked)).toSet
    val newReactioners: Set[AbstractReaction] = (11 to 20).map(nb => FBReaction(FBFrom(s"user$nb", s"name$nb"), PostWhoLiked)).toSet
    assert(oldReactioners != newReactioners)

    val oldDataTypes = Map[DataType, Int](PostGeolocation -> 2, PostWhoCommented -> 4, PostWhoReacted -> 13) // linked with below counts
    val newDataTypes = List[(DataType, Int)]((PostWhoReacted, 7), (PageLikeNumber -> 17)) // prime number is important to test the rounding
    val newItemsSummaries = newDataTypes.flatMap {
      case (tpe, count) => (1 to count).map {
        any => ItemSummary(userId = userId, itemId = s"item$userId", itemType = PostType, dataTypes = Set(tpe), dataCount = 1)
      }
    }

    val expectedOrderCount = 17 - (17 % QuestionGenerationConfig.orderingItemsNumber)

    val oldQuestionCounts = Map[QuestionKind, Int]((Geolocation, 2), (MultipleChoice, 17))

    val userSummary = UserSummary(userId = userId, dataTypeCounts = oldDataTypes, questionCounts = oldQuestionCounts,
      reactioners = oldReactioners, friends = Set())

    val newUserSummary = DataAnalyser.userSummaryWithNewCounts(newReactioners, newItemsSummaries, Set(), userSummary)

    assert(userSummary.userId == newUserSummary.userId)
    assert(newUserSummary.reactioners == newReactioners)

    assert(newUserSummary.dataTypeCounts.size == userSummary.dataTypeCounts.size + 1)
    assert(newUserSummary.dataTypeCounts.getOrElse(PostGeolocation, 0) == 2)
    assert(newUserSummary.dataTypeCounts.getOrElse(PostWhoCommented, 0) == 4)
    assert(newUserSummary.dataTypeCounts.getOrElse(PostWhoReacted, 0) == 20)

    assert(newUserSummary.questionCounts.size == userSummary.questionCounts.size + 1)
    assert(newUserSummary.questionCounts.getOrElse(Geolocation, 0) == 2)
    assert(newUserSummary.questionCounts.getOrElse(MultipleChoice, 0) == 24)
    assert(newUserSummary.questionCounts.getOrElse(Order, 0) == expectedOrderCount)
  }

}
