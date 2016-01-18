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

object GameGenerator {

  def props(database: DefaultDB, userId: String): Props =
    Props(new GameGenerator(database, userId))

  case class CreateBoard(accessToken: String, strategy: String) extends RestMessage

  case class InitBoardCreation()

}

class GameGenerator(database: DefaultDB, userId: String) extends Actor with ActorLogging {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

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


  // Awaits feedback from the FetcherService and the tile creators
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

  private def verifyAndAnswer(client: ActorRef, tiles: List[Tile], ack: Boolean, stale: Boolean, strategy: String): Unit = {
    if (tiles.length == 9 && ack) {
      client ! Board(userId, tiles, stale, strategy)
      sender() ! PoisonPill
    }
  }

}
