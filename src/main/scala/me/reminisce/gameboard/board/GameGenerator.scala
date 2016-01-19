package me.reminisce.gameboard.board

import akka.actor._
import me.reminisce.fetching.FetcherService
import me.reminisce.fetching.FetcherService.FetchData
import me.reminisce.gameboard.board.BoardGenerator.{FailedBoardGeneration, FinishedBoardGeneration}
import me.reminisce.gameboard.board.GameGenerator.{CreateBoard, InitBoardCreation}
import me.reminisce.gameboard.board.GameboardEntities.{Board, Tile}
import me.reminisce.server.domain.Domain._
import me.reminisce.server.domain.{Domain, RestMessage}
import reactivemongo.api.DefaultDB

import scala.concurrent.ExecutionContextExecutor

/**
  * Factory for [[me.reminisce.gameboard.board.GameGenerator]] and case classes for message passing
  */
object GameGenerator {

  /**
    * Create a game generator class which operates for user userId and with the help of database db
    * @param database database from which to get data
    * @param userId user for which to generate the game
    * @return props for created game generator
    */
  def props(database: DefaultDB, userId: String): Props =
    Props(new GameGenerator(database, userId))

  case class CreateBoard(accessToken: String, strategy: String) extends RestMessage

  case class InitBoardCreation()

}

/**
  * Coordinates the whole game generation process (generating a board, requesting a data update and sending feedback)
  * @param database database which holds the data
  * @param userId user for which the game is created
  */
class GameGenerator(database: DefaultDB, userId: String) extends Actor with ActorLogging {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  /**
    * Entry point for this actors, it only handles the CreateBoard message which supplies an access token for the data
    * refresh and a strategy for generating the game.
    * @return Nothing
    */
  def receive = {
    case CreateBoard(accessToken, strategy) =>
      val client = sender()
      val creator = getCreatorFromStrategy(strategy)
      val fetcherService = context.actorOf(FetcherService.props(database))
      creator ! InitBoardCreation()
      fetcherService ! FetchData(userId, accessToken)
      context.become(awaitFeedBack(client, creator, List()))
    case x => log.error("GameGenerator received unexpected Message " + x)
  }


  /**
    * Associates a generator to a strategy.
    * @param strategy selected strategy
    * @return a game board generator
    */
  private def getCreatorFromStrategy(strategy: String): ActorRef = strategy match {
    case "uniform" =>
      context.actorOf(Props(new UniformBoardGenerator(database, userId)))
    case "choose" =>
      context.actorOf(Props(new StrategyChooser(database, userId)))
    case "random" =>
      context.actorOf(Props(new FullRandomBoardGenerator(database, userId)))
    case any =>
      context.actorOf(Props(new StrategyChooser(database, userId)))
  }

  /**
    * Awaits feedback from the FetcherService and the tile creators. This is a recursive method whose arguments hold the
    * state of the actor. The following messages are handled:
    * - FinishedBoardGeneration(receivedTiles, strat): board creation is done, verifies if the fetcher acknowledged the
    * request and sends feedback to the client
    * - FailedBoardGeneration(message): sends an internal error to the client
    * - Done(message): the fetcher acknowledged the request and the token is valid. Verify that the board has been generated
    * and report to the client.
    * - Domain.TooManyRequests(message): the fetcher acknowledges the request but the refresh is already in progress.
    * Verify that the board has been generated and report to the client.
    * - GraphAPIInvalidToken(message): the fetcher acknowledges the request but the token is invalid. Verify that the
    * board has been generated and report to the client.
    * - GraphAPIUnreachable(message): the fetcher acknowledges the request but the Graph API is unreachable. Verify that
    * the board has been generated and report to the client.
    * - AlreadyFresh(message): the fetcher acknowledges the request but the data is fresh. Verify that the board has
    * been generated and report to the client.
    * @param client game requester
    * @param worker game board generator worker
    * @param tiles generated tiles
    * @param fetcherAcked has the fetcher acknowleged
    * @param isTokenStale is the token invalid
    * @param strategy chosen strategy (reported by the worker)
    * @return Nothing
    */
  private def awaitFeedBack(client: ActorRef, worker: ActorRef, tiles: List[Tile],
                            fetcherAcked: Boolean = false, isTokenStale: Boolean = false, strategy: String = ""): Receive = {
    case FinishedBoardGeneration(receivedTiles, strat) =>
      context.become(awaitFeedBack(client, worker, receivedTiles, fetcherAcked, isTokenStale, strat))
      verifyAndAnswer(client, receivedTiles, fetcherAcked, isTokenStale, strat)
    case FailedBoardGeneration(message) =>
      worker ! PoisonPill
      client ! InternalError(message)
      log.error(s"An internal error occurred while generating the gameboard for user $userId.")
    case Done(message) =>
      verifyAndAnswer(client, tiles, ack = true, isTokenStale, strategy)
      context.become(awaitFeedBack(client, worker, tiles, fetcherAcked = true, isTokenStale, strategy))
      log.info(s"Update done. $message")
    case Domain.TooManyRequests(message) =>
      verifyAndAnswer(client, tiles, ack = true, isTokenStale, strategy)
      context.become(awaitFeedBack(client, worker, tiles, fetcherAcked = true, isTokenStale, strategy))
      log.info(message)
    case GraphAPIInvalidToken(message) =>
      verifyAndAnswer(client, tiles, ack = true, stale = true, strategy)
      context.become(awaitFeedBack(client, worker, tiles, fetcherAcked = true, isTokenStale = true, strategy))
      log.info(message)
    case GraphAPIUnreachable(message) =>
      verifyAndAnswer(client, tiles, ack = true, isTokenStale, strategy)
      context.become(awaitFeedBack(client, worker, tiles, fetcherAcked = true, isTokenStale, strategy))
      log.info(message)
    case AlreadyFresh(message) =>
      verifyAndAnswer(client, tiles, ack = true, isTokenStale, strategy)
      context.become(awaitFeedBack(client, worker, tiles, fetcherAcked = true, isTokenStale, strategy))
      log.info(message)
    case any =>
      log.error(s"GameGenerator received an unknown message : $any.")
  }

  /**
    * Verifies that both the game board was generated and the fetcher acknowledged the answer. If so, sends the answer
    * to the client.
    * @param client game requester
    * @param tiles generated tiles
    * @param ack has the fetcher acknowledged
    * @param stale is the token invalid
    * @param strategy chosen strategy
    */
  private def verifyAndAnswer(client: ActorRef, tiles: List[Tile], ack: Boolean, stale: Boolean, strategy: String): Unit = {
    if (tiles.length == 9 && ack) {
      client ! Board(userId, tiles, stale, strategy)
      sender() ! PoisonPill
    }
  }

}
