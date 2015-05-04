package me.reminisce.service.gameboardgen

import akka.actor.ActorRef
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBPageLike, PostQuestions, UserStat}
import me.reminisce.service.gameboardgen.BoardGenerator.FailedBoardGeneration
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.tilegen.TileGenerator
import me.reminisce.service.gameboardgen.tilegen.TileGenerator.{CreateTile, FailedTileCreation, FinishedTileCreation}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.Random

object RandomBoardGenerator {

}

class RandomBoardGenerator(database: DefaultDB, user_id: String) extends BoardGenerator(database, user_id) {

  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    val selector = BSONDocument("user_id" -> user_id)
    findOne[UserStat](userCollection, selector, client) {
      case Some(userStat) =>
        if (userStat.question_counts.contains("MCWhichPageDidYouLike")) {
          generateRandomBoardWithPages(client)
        } else {
          generateRandomBoard(client)
        }
      case None =>
        generateRandomBoard(client)
    }
  }


  def handleTileCreated(client: ActorRef, finish: FinishedTileCreation): Unit = {
    client ! FinishedTileCreation(finish.user_id, finish.tile)
  }

  def handleTileFailed(client: ActorRef, failed: FailedTileCreation): Unit = {
    val message = failed.message
    log.error(s"Failed board creation: $message")
    client ! FailedBoardGeneration(message)
  }

  def generateRandomBoardWithPages(client: ActorRef): Unit = {
    val postSelector = BSONDocument("user_id" -> user_id, "questions_count" -> BSONDocument("$gt" -> 0))
    val postsQuestionsCollection = database[BSONCollection](MongoDatabaseService.postQuestionsCollection)
    findSome[PostQuestions](postsQuestionsCollection, postSelector, client) {
      listPostQuestions =>
        val pagesSelector = BSONDocument("user_id" -> user_id)
        val pageLikesCollection = database[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
        findSome[FBPageLike](pageLikesCollection, pagesSelector, client) {
          listPageLikes =>
            val listPageLikesReformed = listPageLikes.map(pageLike => pageLike.page_id -> List("MCWhichPageDidYouLike"))
            val listPostQuestionsReformed = listPostQuestions.map(postQuestion => postQuestion.post_id -> postQuestion.questions)
            val fullQuestionChoice = listPageLikesReformed ++ listPostQuestionsReformed
            generateWithListOfChoices(client, fullQuestionChoice)
        }
    }
  }

  def generateRandomBoard(client: ActorRef): Unit = {
    val selector = BSONDocument("user_id" -> user_id, "questions_count" -> BSONDocument("$gt" -> 0))
    val postsQuestionsCollection = database[BSONCollection](MongoDatabaseService.postQuestionsCollection)
    findSome[PostQuestions](postsQuestionsCollection, selector, client) {
      listPostQuestions =>
        val listPostQuestionsReformed = listPostQuestions.map(postQuestion => postQuestion.post_id -> postQuestion.questions)
        generateWithListOfChoices(client, listPostQuestionsReformed)
    }
  }

  def generateWithListOfChoices(client: ActorRef, choices: List[(String, List[String])]): Unit = {
    if (choices.length >= 27) {
      val shuffledList = Random.shuffle(choices).take(27)
      val preparedQuestions = prepareQuestions(shuffledList)
      val mappedWithKind = mapToKind(preparedQuestions)
      generateTiles(mappedWithKind)
    }
    else {
      val moreChoices: List[(String, List[String])] = choices.flatMap {
        case (itemId, list) => list.map(elm => itemId -> List(elm))
      }
      if (moreChoices.length >= 27) {
        val shuffledList = Random.shuffle(moreChoices).take(27)
        val preparedQuestions = prepareQuestions(shuffledList)
        val mappedWithKind = mapToKind(preparedQuestions)
        generateTiles(mappedWithKind)
      } else {
        client ! FailedBoardGeneration(s"Failed to generate board for user $user_id : not enough data.")
      }

    }

    def prepareQuestions(shuffledChoices: List[(String, List[String])]): List[(String, String)] = {
      shuffledChoices.map {
        case (itemId, list) => {
          val len = list.length
          (itemId, list(Random.nextInt(len)))
        }
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

    def generateTiles(questions: List[(QuestionKind, (String, String))]): Unit = questions match {
      case Nil =>
      case qList =>
        qList.groupBy(_._1).toList.filter(elm => elm._2.length >= 3) match {
          case List() =>
            val (selected, next) = questions.splitAt(3)
            val worker = context.actorOf(TileGenerator.props(database))
            worker ! CreateTile(user_id, selected.map(_._2))
            generateTiles(next)
          case someQs =>
            val selectedQuestions = someQs.head._2.take(3)
            val kind = selectedQuestions.head._1
            val worker = context.actorOf(TileGenerator.props(database))
            worker ! CreateTile(user_id, selectedQuestions.map(_._2), kind)
            generateTiles(questions.filterNot(selectedQuestions.toSet))
        }

    }
  }
}
