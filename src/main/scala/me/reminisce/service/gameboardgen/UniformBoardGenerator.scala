package me.reminisce.service.gameboardgen

import akka.actor.ActorRef
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.StatsEntities.UserStats
import me.reminisce.service.gameboardgen.BoardGenerator._
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.stats.StatsDataTypes.DataType
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

object UniformBoardGenerator

class UniformBoardGenerator(database: DefaultDB, userId: String) extends RandomBoardGenerator(database, userId) {

  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    val selector = BSONDocument("userId" -> userId)
    findOne[UserStats](userCollection, selector, client) {
      case Some(userStats) =>
        generateBoard(userStats, client)(drawUniformlyFromBags[QuestionKind], drawItemsAtRandomFromBags[DataType])
      case None =>
        client ! FailedBoardGeneration(s"Failed to generate board for user $userId : no UserStats.")
    }
  }

}
