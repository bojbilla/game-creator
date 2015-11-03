package me.reminisce.service.gameboardgen

import akka.actor.ActorRef
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.StatsEntities.{ItemStats, UserStats}
import me.reminisce.service.gameboardgen.BoardGenerator.{FailedBoardGeneration, FinishedBoardGeneration}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.Tile
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerationConfig
import me.reminisce.service.gameboardgen.tilegen.TileGenerator
import me.reminisce.service.gameboardgen.tilegen.TileGenerator.{CreateTile, FailedTileCreation, FinishedTileCreation}
import me.reminisce.service.stats.StatsDataTypes.{DataType, _}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.Random

object UniformBoardGenerator {

}

class UniformBoardGenerator(database: DefaultDB, userId: String) extends BoardGenerator(database, userId) {

  var tiles: List[Tile] = List()

  val orderingItemsNumber = QuestionGenerationConfig.orderingItemsNumber

  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    val selector = BSONDocument("userId" -> userId)
    findOne[UserStats](userCollection, selector, client) {
      case Some(userStats) =>
        generateBoard(userStats, client)
      case None =>
        client ! FailedBoardGeneration(s"Failed to generate board for user $userId : no UserStats.")
    }
  }

  def awaitFeedBack(client: ActorRef): Receive = {
    case FinishedTileCreation(usrId, tile) =>
      tiles = tile :: tiles
      if (tiles.length == 9) {
        tiles = Random.shuffle(tiles)
        client ! FinishedBoardGeneration(tiles)
      }
    case FailedTileCreation(message) =>
      log.error(s"Failed board creation: $message")
      client ! FailedBoardGeneration(message)
  }


  def generateBoard(userStats: UserStats, client: ActorRef): Unit = {
    // An order question is made of multiple items
    val normalizedCounts = userStats.questionCounts.map {
      case (k, v) =>
        if (k == Order.toString) {
          k -> v / orderingItemsNumber
        } else {
          k -> v
        }
    }

    val totalCount = normalizedCounts.toList.map { case (k, v) => v }.sum

    if (totalCount >= 27) {
      val tlCount = normalizedCounts.getOrElse(Timeline.toString, 0)
      val ordCount = normalizedCounts.getOrElse(Order.toString, 0)
      val mcCount = normalizedCounts.getOrElse(MultipleChoice.toString, 0)
      val geoCount = normalizedCounts.getOrElse(Geolocation.toString, 0)

      val selectedKinds = drawUniformlyFromBags[QuestionKind](List(tlCount, ordCount, mcCount, geoCount),
        List(Timeline, Order, MultipleChoice, Geolocation), 27)



      val pairsKindType: List[(QuestionKind, DataType)] = selectedKinds.groupBy(el => el).toList.flatMap {
        case (kind, kindList) =>
          val possTypes = possibleTypes(kind)
          val counts = possTypes.map(t => userStats.dataTypeCounts.getOrElse(t.name, 0))
          val selectedTypes =
            if (kind == Order) {
              drawItemsAtRandomFromBags[DataType](counts, possTypes, kindList.length, orderingItemsNumber)
            } else {
              drawItemsAtRandomFromBags[DataType](counts, possTypes, kindList.length)
            }
          kindList.zip(selectedTypes)
      }

      generateWithKindTypePairs(List(), pairsKindType, client)

    } else {
      client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
    }

  }

  def generateWithKindTypePairs(alreadyFound: List[(QuestionKind, DataType, List[(String, String)])],
                                pairsKindType: List[(QuestionKind, DataType)], client: ActorRef): Unit = pairsKindType match {
    case head :: tail =>
      val (current, rest) = pairsKindType.partition(el => (el._1 == head._1) && (el._2 == head._2))
      val itemsStatsCollection = database[BSONCollection](MongoDatabaseService.itemsStatsCollection)
      val query = BSONDocument("userId" -> userId, "dataTypes" -> BSONDocument("$in" -> List(head._2.name)), "readForStats" -> true)
      findSome[ItemStats](itemsStatsCollection, query, client) {
        listItemsStats =>
          if (head._1 == Order) {
            if (listItemsStats.length >= orderingItemsNumber * current.length) {
              val groups = listItemsStats.groupBy(is => is.itemType).toList.map { case (itemType, list) => list.map(is => (is.itemId, is.itemType)) }
              val randomBuckets = Random.shuffle(groups.flatMap(list => createBuckets[(String, String)](list, orderingItemsNumber)))
              if (randomBuckets.length >= current.length) {
                val newFound = current.zip(randomBuckets).map {
                  case (k, v) => (k._1, k._2, v)
                }
                generateWithKindTypePairs(alreadyFound ++ newFound, rest, client)
              } else {
                client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
              }
            } else {
              client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
            }
          } else {
            if (listItemsStats.length >= current.length) {
              val newFound = current.zip(listItemsStats.map(is => (is.itemId, is.itemType))).map {
                case (k, v) => (k._1, k._2, List(v))
              }
              generateWithKindTypePairs(alreadyFound ++ newFound, rest, client)
            } else {
              client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
            }
          }
      }
    case List() =>
      val tiles = generateTiles(alreadyFound, client)
      if (tiles.length > 9) {
        client ! FailedBoardGeneration(s"Too many tiles generated ! (${tiles.length})")
      }
      context.become(awaitFeedBack(client))
      tiles.foreach {
        tile =>
          val worker = context.actorOf(TileGenerator.props(database))
          val req = CreateTile(userId, tile._2, tile._1)
          worker ! req
      }

  }


}
