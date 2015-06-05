package me.reminisce.service.gameboardgen

import akka.actor._
import me.reminisce.fetcher.FetcherService
import me.reminisce.fetcher.FetcherService.FetchData
import me.reminisce.server.domain.Domain._
import me.reminisce.server.domain.{Domain, RestMessage}
import me.reminisce.service.gameboardgen.BoardGenerator.{FailedBoardGeneration, FinishedBoardGeneration}
import me.reminisce.service.gameboardgen.GameGenerator.{CreateBoard, InitBoardCreation}
import me.reminisce.service.gameboardgen.GameboardEntities.{Board, Tile}
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

  var tiles: List[Tile] = List()
  var fetcherAcked = false
  var isTokenStale = false

  def receive = {
    case CreateBoard(accessToken, strategy) =>
      val client = sender()
      val creator = getCreatorFromStrategy(strategy)
      val fetcherService = context.actorOf(FetcherService.props(database))
      creator ! InitBoardCreation()
      fetcherService ! FetchData(userId, accessToken)
      context.become(awaitFeedBack(client, creator))
    case x => log.error("GameGenerator received unexpected Message " + x)
  }


  def getCreatorFromStrategy(strategy: String): ActorRef = strategy match {
    case "choose" =>
      context.actorOf(Props(new StrategyChooser(database, userId)))
    case "random" =>
      context.actorOf(Props(new RandomBoardGenerator(database, userId)))
    case any =>
      context.actorOf(Props(new RandomBoardGenerator(database, userId)))
  }


  // Awaits feedback from the FetcherService and the tile creators
  def awaitFeedBack(client: ActorRef, worker: ActorRef): Receive = {
    case FinishedBoardGeneration(receivedTiles) =>
      tiles = receivedTiles
      verifyAndAnswer(client)
    case FailedBoardGeneration(message) =>
      worker ! PoisonPill
      client ! InternalError(message)
      log.error(s"An internal error occurred while serving the request.")
    case Done(message) =>
      fetcherAcked = true
      verifyAndAnswer(client)
      log.info(s"Update done. $message")
    case Domain.TooManyRequests(message) =>
      fetcherAcked = true
      verifyAndAnswer(client)
      log.info(message)
    case GraphAPIInvalidToken(message) =>
      isTokenStale = true
      fetcherAcked = true
      verifyAndAnswer(client)
      log.error(message)
    case GraphAPIUnreachable(message) =>
      fetcherAcked = true
      verifyAndAnswer(client)
      log.error(message)
    case AlreadyFresh(message) =>
      fetcherAcked = true
      verifyAndAnswer(client)
      log.info(message)
  }

  def verifyAndAnswer(client: ActorRef): Unit = {
    if (tiles.length == 9 && fetcherAcked) {
      client ! Board(userId, tiles, isTokenStale)
      sender() ! PoisonPill
    }
  }

}
