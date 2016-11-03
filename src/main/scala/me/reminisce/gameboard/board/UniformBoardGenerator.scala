package me.reminisce.gameboard.board

import akka.actor.ActorRef
import me.reminisce.analysis.DataTypes.DataType
import me.reminisce.database.AnalysisEntities.UserSummary
import me.reminisce.database.MongoCollections
import me.reminisce.gameboard.board.BoardGenerator.{FailedBoardGeneration, drawItemsAtRandomFromBags, drawUniformlyFromBags}
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind.QuestionKind
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

object UniformBoardGenerator

/**
  * Implementation of a random board generator where we draw the question kind in a round robin fashion instead of a
  * truly random strategy (see [[me.reminisce.gameboard.board.BoardGenerator.drawUniformlyFromBags]])
  *
  * @param database database in which the data is stored
  * @param userId   user for which the board is generated
  */
class UniformBoardGenerator(database: DefaultDB, userId: String) extends RandomBoardGenerator(database, userId, "uniform") {

  /**
    * Creates the game, aborts if there is no user summary.
    *
    * @param client the board requester
    */
  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoCollections.userSummaries)
    val selector = BSONDocument("userId" -> userId)
    findOne[UserSummary](userCollection, selector, client) {
      case Some(userSummary) =>
        generateBoard(userSummary, client)(drawUniformlyFromBags[QuestionKind], drawItemsAtRandomFromBags[DataType])
      case None =>
        client ! FailedBoardGeneration(s"Failed to generate board for user $userId : no UserSummary.")
    }
  }

}
