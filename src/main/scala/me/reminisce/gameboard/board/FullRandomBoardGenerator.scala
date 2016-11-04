package me.reminisce.gameboard.board

import akka.actor.ActorRef
import me.reminisce.analysis.DataTypes._
import me.reminisce.database.AnalysisEntities.{ItemSummary, UserSummary}
import me.reminisce.database.MongoCollections
import me.reminisce.gameboard.board.BoardGenerator._
import me.reminisce.gameboard.board.GameboardEntities.{Order, QuestionKind}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.{BSONArray, BSONDocument}

import scala.util.Random

object FullRandomBoardGenerator

/**
  * Implementation of a random board generator using fully random drawers
  *
  * @param database database in which the data is stored
  * @param userId   user for which the board is generated
  */
class FullRandomBoardGenerator(database: DefaultDB, userId: String) extends RandomBoardGenerator(database, userId, "random") {

  /**
    * Create the game board and chooses the right generateBoard method to call depending on the existence of user
    * summary or not
    *
    * @param client the board requester
    */
  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoCollections.userSummaries)
    val selector = BSONDocument("userId" -> userId)
    findOne[UserSummary](userCollection, selector, client) {
      case Some(userSummary) =>
        generateBoard(userSummary, client)(drawItemsAtRandomFromBags[QuestionKind], drawItemsAtRandomFromBags[DataType])
      case None =>
        generateBoard(client)
    }
  }

  /**
    * Implementation of the generate board method (see [[me.reminisce.gameboard.board.RandomBoardGenerator.generateBoard]])
    * without having user summary
    *
    * @param client original board requester
    */
  private def generateBoard(client: ActorRef): Unit = {
    // As order cannot be generated only from one question, we exclude the types that only lead to this kind of question
    // (because we cannot have a guarantee of the number of available items as we do not have user summary)
    // PostWhoReacted is also excluded as it cannot be generated without UserSummary (even though this type should not be
    // marked as available on an item if no UserSummary was generated)
    val excluded = PostWhoReacted :: possibleTypes(Order).filter(t => possibleKind(t).size == 1)
    val selector = BSONDocument("userId" -> userId,
      "$or" -> BSONArray(
        BSONDocument("dataCount" -> BSONDocument("$gt" -> excluded.size)),
        BSONDocument("dataTypes" -> BSONDocument("$nin" -> excluded.map(_.name)))))
    val itemsSummariesCollection = database[BSONCollection](MongoCollections.itemsSummaries)
    findSomeRandom[ItemSummary](itemsSummariesCollection, selector, client) {
      itemsSummaries =>
        if (itemsSummaries.length < 27) {
          client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
        } else {
          val shuffledList = Random.shuffle(itemsSummaries).take(27)
          val tuples = shuffledList.flatMap {
            is =>
              val okTypes = is.dataTypes.filterNot(t => excluded.contains(t))
              for {
                chosenType <- Random.shuffle(okTypes).headOption
                chosenKind <- Random.shuffle(possibleKind(chosenType).filterNot(k => k == Order)).headOption
              } yield (chosenKind, chosenType, List((is.itemId, is.itemType)))
          }

          sendOrders(client, tuples)
        }
    }
  }

}
