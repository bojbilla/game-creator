package me.reminisce.service.gameboardgen

import akka.actor.ActorRef
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBPageLike, PostQuestions, UserStats}
import me.reminisce.service.gameboardgen.BoardGenerator.{FailedBoardGeneration, FinishedBoardGeneration}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{SpecificQuestionType, Tile}
import me.reminisce.service.gameboardgen.tilegen.TileGenerator
import me.reminisce.service.gameboardgen.tilegen.TileGenerator.{CreateTile, FailedTileCreation, FinishedTileCreation}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONArray, BSONDocument}

import scala.util.Random

object RandomBoardGenerator {

}

class RandomBoardGenerator(database: DefaultDB, userId: String) extends BoardGenerator(database, userId) {

  var tiles: List[Tile] = List()

  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    val selector = BSONDocument("userId" -> userId)
    findOne[UserStats](userCollection, selector, client) {
      case Some(userStats) =>
        if (userStats.questionCounts.contains("MCWhichPageDidYouLike")) {
          generateRandomBoardWithPages(client)
        } else {
          generateRandomBoard(client)
        }
      case None =>
        generateRandomBoard(client)
    }
  }

  def awaitFeedBack(client: ActorRef): Receive = {
    case FinishedTileCreation(usrId, tile) =>
      tiles = tile :: tiles
      if (tiles.length == 9) {
        tiles = Random.shuffle(tiles)
        client ! FinishedBoardGeneration(tiles)
      }
    case FailedTileCreation(message) =>
      log.error(s"Failed board creation: $message")
      client ! FailedBoardGeneration(message)
  }

  def generateRandomBoardWithPages(client: ActorRef): Unit = {
    val postSelector = BSONDocument("userId" -> userId, "questionsCount" -> BSONDocument("$gt" -> 0))
    val postsQuestionsCollection = database[BSONCollection](MongoDatabaseService.postQuestionsCollection)
    findSome[PostQuestions](postsQuestionsCollection, postSelector, client) {
      listPostQuestions =>
        val pagesSelector = BSONDocument("userId" -> userId)
        val pageLikesCollection = database[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
        findSome[FBPageLike](pageLikesCollection, pagesSelector, client) {
          listPageLikes =>
            val listPageLikesReformed = listPageLikes.map(pageLike => pageLike.pageId -> List("MCWhichPageDidYouLike"))
            val listPostQuestionsReformed = listPostQuestions.map(postQuestion => postQuestion.postId -> postQuestion.questions)
            val fullQuestionChoice = listPageLikesReformed ++ listPostQuestionsReformed
            generateWithListOfChoices(client, fullQuestionChoice)
        }
    }
  }

  def generateRandomBoard(client: ActorRef): Unit = {
    //TODO: Temporary measure to avoid geolocation
    val selector = BSONDocument("userId" -> userId,
      "$or" -> BSONArray(
        BSONDocument("questionsCount" -> BSONDocument("$gt" -> 1)),
        BSONDocument("questions" -> BSONDocument("$nin" -> BSONArray("GeoWhatCoordinatesWereYouAt")))))
    val postsQuestionsCollection = database[BSONCollection](MongoDatabaseService.postQuestionsCollection)
    findSome[PostQuestions](postsQuestionsCollection, selector, client) {
      listPostQuestions =>
        val listPostQuestionsReformed = listPostQuestions.map(postQuestion => postQuestion.postId -> postQuestion.questions)
        generateWithListOfChoices(client, listPostQuestionsReformed)
    }
  }

  def generateWithListOfChoices(client: ActorRef, choices: List[(String, List[String])]): Unit = {
    if (choices.length >= 27) {
      val shuffledList = Random.shuffle(choices).take(27)
      val preparedQuestions = prepareQuestions(shuffledList)
      val mappedWithKind = mapToKind(preparedQuestions)
      generateTiles(mappedWithKind)
    } else {
      //TODO: Temporary measure to avoid geolocation
      val moreChoices: List[(String, List[String])] = choices.flatMap {
        case (itemId, list) => list.filter(s => s != "GeoWhatCoordinatesWereYouAt").map(elm => itemId -> List(elm))
      }
      if (moreChoices.length >= 27) {
        val shuffledList = Random.shuffle(moreChoices).take(27)
        val preparedQuestions = prepareQuestions(shuffledList)
        val mappedWithKind = mapToKind(preparedQuestions)
        generateTiles(mappedWithKind)
      } else {
        client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
      }

    }

    def prepareQuestions(shuffledChoices: List[(String, List[String])]): List[(String, String)] = {
      shuffledChoices.map {
        case (itemId, list) =>
          //TODO: Temporary measure to avoid geolocation
          val filteredList = list.filter(s => s != "GeoWhatCoordinatesWereYouAt")
          val len = filteredList.length
          (itemId, filteredList(Random.nextInt(len)))
      }
    }

    def mapToKind(questions: List[(String, String)]): List[(QuestionKind, (String, String))] = {
      def typeToKind(questionType: SpecificQuestionType): QuestionKind = questionType match {
        case MCWhichPageDidYouLike | MCWhoLikedYourPost | MCWhoMadeThisCommentOnYourPost => MultipleChoice
        case TLWhenDidYouShareThisPost => Timeline
        case GeoWhatCoordinatesWereYouAt => Geolocation
      }
      questions.map {
        q =>
          (typeToKind(SpecificQuestionType.withName(q._2)), q)
      }
    }

    def generateTiles(questions: List[(QuestionKind, (String, String))]): Unit = {
      def generateRequests(questions: List[(QuestionKind, (String, String))]): List[CreateTile] =
        questions match {
          case Nil => List()
          case qList =>
            qList.groupBy(_._1).toList.filter(elm => elm._2.length >= 3) match {
              case List() =>
                val (selected, next) = questions.splitAt(3)
                CreateTile(userId, selected.map(_._2)) :: generateRequests(next)
              case someQs =>
                val selectedQuestions = someQs.head._2.take(3)
                val kind = selectedQuestions.head._1
                CreateTile(userId, selectedQuestions.map(_._2), kind) :: generateRequests(questions.filterNot(selectedQuestions.toSet))
            }

        }
      context.become(awaitFeedBack(client))
      generateRequests(questions).foreach {
        req =>
          val worker = context.actorOf(TileGenerator.props(database))
          worker ! req
      }
    }
  }
}
