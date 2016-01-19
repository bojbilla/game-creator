package me.reminisce.gameboard.board

import akka.actor.{ActorRef, Props}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.database.StatsEntities.UserStats
import me.reminisce.gameboard.board.BoardGenerator.{FailedBoardGeneration, FinishedBoardGeneration}
import me.reminisce.gameboard.board.GameGenerator.InitBoardCreation
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument


object StrategyChooser

/**
  * Board generator which picks another generator to generate the game board
  * @param database database in which the data is stored
  * @param userId user for which the board is generated
  */
class StrategyChooser(database: DefaultDB, userId: String) extends BoardGenerator(database, userId) {

  /**
    * Create the game by first choosing another generator and then requesting it to create a game board
    * @param client the board requester
    */
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

  /**
    * Chooses a game board generator based on the (found or not) user statistics
    * @param userStatsOpt an option of user statistics
    * @return referece to the chosen generator
    */
  private def getCreatorFromUserStats(userStatsOpt: Option[UserStats]): ActorRef = userStatsOpt match {
    case None =>
      log.info(s"Random generator chosen for user $userId.")
      context.actorOf(Props(new FullRandomBoardGenerator(database, userId)))
    case Some(userStats) =>
      log.info(s"Uniform generator chosen for user $userId.")
      context.actorOf(Props(new UniformBoardGenerator(database, userId)))
  }

  /**
    * Waits fot the chosen board generator to finish the board creation. The messages are just forwarded to the
    * original requester.
    * @param client original board requester
    * @return Nothing
    */
  private def awaitFeedBack(client: ActorRef): Receive = {
    case FinishedBoardGeneration(tiles, strat) =>
      client ! FinishedBoardGeneration(tiles, "chooser/" + strat)
    case FailedBoardGeneration(message) =>
      client ! FailedBoardGeneration(message)
    case any =>
      log.error(s"StrategyChooser received an unexpected message: $any.")
  }

}