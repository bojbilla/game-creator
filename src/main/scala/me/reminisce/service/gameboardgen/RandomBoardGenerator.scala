package me.reminisce.service.gameboardgen

import akka.actor.ActorRef
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBPageLike, PostQuestions, UserStat}
import me.reminisce.service.gameboardgen.BoardGenerator.FailedBoardGeneration
import me.reminisce.service.gameboardgen.tilegen.TileGenerator
import me.reminisce.service.gameboardgen.tilegen.TileGenerator.{CreateTile, CreateTimelineTile, FailedTileCreation, FinishedTileCreation}
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

  def awaitFeedBack(client: ActorRef): Receive = {
    case FinishedTileCreation(userId, tile) =>
      client ! FinishedTileCreation(userId, tile)
    case FailedTileCreation(message) =>
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
      val shuffledList = Random.shuffle(choices)
      generateTiles(shuffledList, 9)
    }
    else {
      val moreChoices: List[(String, List[String])] = choices.flatMap {
        case (itemId, list) => list.map(elm => itemId -> List(elm))
      }
      if (moreChoices.length >= 27) {
        val shuffledList = Random.shuffle(moreChoices)
        generateTiles(shuffledList, 9)
      } else {
        client ! FailedBoardGeneration(s"Failed to generate board for user $user_id : not enough data.")
      }

    }

    def generateTiles(shuffledChoices: List[(String, List[String])], depth: Int): Unit = {
      if (depth > 0) {
        val (selected, next) = shuffledChoices.splitAt(3)
        val questions = selected.map {
          case (itemId, list) => {
            val len = list.length
            (itemId, list(Random.nextInt(len)))
          }
        }
        val worker = context.actorOf(TileGenerator.props(database))
        worker ! CreateTile(user_id, questions)
        generateTiles(next, depth - 1)
      } else {
        context.become(awaitFeedBack(client))
      }
    }
  }

  def dummyGeneration(client: ActorRef) = {
    for (i <- 1 to 9) {
      val worker = context.actorOf(TileGenerator.props(database))
      worker ! CreateTimelineTile(user_id)
    }
    context.become(awaitFeedBack(client))
  }
}
