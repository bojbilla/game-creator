package me.reminisce.server


import akka.actor._
import me.reminisce.analysis.DataAnalyser
import me.reminisce.analysis.DataAnalyser.NewBlackList
import me.reminisce.database.AnalysisEntities.UserSummary
import me.reminisce.database.DeletionService.{ClearDatabase, RemoveUser}
import me.reminisce.database.MongoDBEntities.FBFrom
import me.reminisce.database.{DeletionService, MongoCollections}
import me.reminisce.fetching.FetcherService
import me.reminisce.fetching.FetcherService.FetchData
import me.reminisce.gameboard.board.GameGenerator
import me.reminisce.gameboard.board.GameGenerator.CreateBoard
import me.reminisce.server.domain.{Domain, RESTHandlerCreator, RestMessage}
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{DefaultDB, MongoConnection}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.StatusCodes.{InternalServerError, NotFound}
import spray.http._
import spray.httpx.Json4sSupport
import spray.httpx.marshalling.ToResponseMarshaller
import spray.routing._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object GameCreatorService

/**
  * Defines a generic GameCreatorServiceActor
  */
trait GameCreatorServiceActor extends GameCreatorService {
  def actorRefFactory = context

  def receive = runRoute(gameCreatorRoutes)
}

/**
  * Defines a GameCreatorService with the handled routes.
  */
trait GameCreatorService extends HttpService with RESTHandlerCreator with Actor with ActorLogging with Json4sSupport {
  val dbName: String
  val dbConnection: MongoConnection
  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )
  val gameCreatorRoutes = {

    path("fetchData") {
      get {
        parameters('user_id.as[String], 'access_token.as[String]) {
          (userId: String, accessToken: String) =>
            fetchData {
              FetchData(userId, accessToken)
            }
        }
      }
    } ~ path("gameboard") {
      get {
        parameters("user_id", "access_token", "strategy" ? "choose") {
          (userId: String, accessToken: String, strategy: String) =>
            createBoard(CreateBoard(accessToken, strategy), userId)
        }
      }
    } ~ path("info") {
      get {
        parameters("UNUSED" ? "") {
          //ugly fix
          (UNUSED: String) =>
            // IntelliJ does not manage to find the reference to BuildInfo which is a class generated by
            // the sbtBuildInfo package (https://github.com/sbt/sbt-buildinfo) at compile time but sbt finds it,
            // do not import what IntelliJ suggests to import
            complete(BuildInfo.toMap + ("appMode" -> ApplicationConfiguration.appMode))
        }
      }
    } ~ path("blacklist") {
      get {
        parameters('user_id.as[String]) {
          (userId: String) =>
            getBlackList(userId)
        }
      } ~
        post {
          parameters('user_id.as[String]) {
            (userId: String) =>
              extract(_.request.headers)
              entity(as[List[FBFrom]]) {
                blacklist =>
                  setBlackList(blacklist, userId)
              }
          }
        }
    } ~ path("reactions") {
      get {
        parameters('user_id.as[String]) {
          (userId: String) =>
            getReactions(userId)
        }
      }
    } ~ path("removeUser") {
      delete {
        parameters("user_id") {
          (userId: String) =>
            removeUser(RemoveUser(userId), userId)
        }
      }
    } ~ path("dropDatabase") {
      delete {
        parameters("UNUSED" ? "") {
          //ugly fix
          (UNUSED: String) =>
            dropDatabase(ClearDatabase())
        }
      }
    }
  }

  def actorRefFactory: ActorContext

  /**
    * Handles board creation requests
    *
    * @param message rest message to request a gameboard to the GameGenerator
    * @param userId  user for which a board was requested
    * @return a spray route
    */
  private def createBoard(message: RestMessage, userId: String): Route = {
    handleWithDb {
      (db, ctx) =>
        log.info(s"Creating game board for user $userId.")
        val generator = context.actorOf(GameGenerator.props(db, userId))
        perRequest(ctx, generator, message)
    }
  }

  private def handleWithDb(handler: (DefaultDB, RequestContext) => Unit): Route = {
    ctx =>
      dbConnection.database(dbName).onComplete {
        case Success(db) =>
          handler(db, ctx)
        case Failure(e) =>
          complete(InternalServerError, s"${e.getMessage}")
      }
  }

  /**
    * Handles fetching requests
    *
    * @param message rest message to request a data fetch to the FetcherService
    * @return a spray route
    */
  private def fetchData(message: RestMessage): Route = {
    handleWithDb {
      (db, ctx) =>
        val fetcherService = context.actorOf(FetcherService.props(db))
        perRequest(ctx, fetcherService, message)
    }
  }

  /**
    * Handles requests to remove a user from the database
    *
    * @param message the message to request the deletion to the deletion service
    * @param userId  user to remove fro mthe database
    * @return a spray route
    */
  private def removeUser(message: RestMessage, userId: String): Route = {
    handleWithDb {
      (db, ctx) =>
        val deletionService = context.actorOf(DeletionService.props(db))
        perRequest(ctx, deletionService, message)
    }
  }

  /**
    * Handles requests to drop the database
    *
    * @param message the rest message to request
    * @return
    */
  private def dropDatabase(message: RestMessage): Route = {
    handleWithDb {
      (db, ctx) =>
        val deletionService = context.actorOf(DeletionService.props(db))
        perRequest(ctx, deletionService, message)
    }
  }

  private def setBlackList(blacklist: List[FBFrom], userId: String): Route = {
    handleWithDb {
      (db, ctx) =>
        val dataAnalyser = context.actorOf(DataAnalyser.props(userId, db))
        val blackListMessage = NewBlackList(blacklist.toSet)
        perRequest(ctx, dataAnalyser, blackListMessage)
    }
  }

  private def getBlackList(userId: String): Route = {
    val selector = BSONDocument("userId" -> userId)
    doGetData[UserSummary, Set[FBFrom]](userId, selector, MongoCollections.userSummaries, "blacklist") {
      summary =>
        summary.blacklist.getOrElse(Set())
    }
  }

  private def getReactions(userId: String): Route = {
    val selector = BSONDocument("userId" -> userId)
    doGetData[UserSummary, Set[FBFrom]](userId, selector, MongoCollections.userSummaries, "reactioners") {
      summary =>
        summary.reactioners.map(_.from)
    }
  }

  private def doGetData[DBData, ReturnData](userId: String, selector: BSONDocument, collectionName: String, dataPrettyName: String = "data")
                                           (extractor: DBData => ReturnData)
                                           (implicit reader: BSONDocumentReader[DBData],
                                            marshaller: ToResponseMarshaller[ReturnData]): Route = {
    handleWithDb {
      (db, ctx) =>
        log.info(s"Getting $dataPrettyName for $userId.")
        val collection = db[BSONCollection](collectionName)
        collection.find(selector).one[DBData].onComplete {
          case Success(maybeDBData) =>
            maybeDBData.fold {
              ctx.complete(NotFound, Domain.NotFound(s"Could not find $dataPrettyName for $userId."))
            } {
              dbData =>
                val retData = extractor(dbData)
                ctx.complete(retData)
            }
          case Failure(e) =>
            log.error(s"Could not get $dataPrettyName : $e.")
            ctx.complete(InternalServerError)
        }
    }
  }

}
