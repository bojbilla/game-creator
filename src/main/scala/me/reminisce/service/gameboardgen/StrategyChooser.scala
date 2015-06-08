package me.reminisce.service.gameboardgen

import akka.actor.{ActorRef, Props}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.UserStats
import me.reminisce.service.gameboardgen.BoardGenerator.{FailedBoardGeneration, FinishedBoardGeneration}
import me.reminisce.service.gameboardgen.GameGenerator.InitBoardCreation
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument


object StrategyChooser {

}

class StrategyChooser(database: DefaultDB, userId: String) extends BoardGenerator(database, userId) {

  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    val selector = BSONDocument("userId" -> userId)
    findOne[UserStats](userCollection, selector, client) {
      userStatsOpt =>
        context.become(awaitFeedBack(client))
        val generator = getCreatorFromUserStats(userStatsOpt)
        generator ! InitBoardCreation()
    }
  }

  def getCreatorFromUserStats(userStatsOpt: Option[UserStats]): ActorRef = userStatsOpt match {
    case None =>
      log.info(s"Random generator chosen for user $userId.")
      context.actorOf(Props(new RandomBoardGenerator(database, userId)))
    case Some(userStats) =>
      log.info(s"Uniform generator chosen $userId.")
      context.actorOf(Props(new UniformBoardGenerator(database, userId)))
  }

  def awaitFeedBack(client: ActorRef): Receive = {
    case FinishedBoardGeneration(tiles) =>
      client ! FinishedBoardGeneration(tiles)
    case FailedBoardGeneration(message) =>
      client ! FailedBoardGeneration(message)
  }

}