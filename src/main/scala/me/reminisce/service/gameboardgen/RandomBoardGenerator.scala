package me.reminisce.service.gameboardgen

import akka.actor.ActorRef
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.StatsEntities.{ItemStats, UserStats}
import me.reminisce.service.gameboardgen.BoardGenerator._
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.Tile
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerationConfig
import me.reminisce.service.gameboardgen.tilegen.TileGenerator
import me.reminisce.service.gameboardgen.tilegen.TileGenerator.{CreateTile, FailedTileCreation, FinishedTileCreation}
import me.reminisce.service.stats.StatsDataTypes._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.Random

object RandomBoardGenerator

abstract class RandomBoardGenerator(database: DefaultDB, userId: String) extends BoardGenerator(database, userId) {

  val orderingItemsNumber = QuestionGenerationConfig.orderingItemsNumber

  protected def awaitFeedBack(client: ActorRef, tiles: List[Tile]): Receive = {
    case FinishedTileCreation(usrId, tile) =>
      val newTiles = tile :: tiles
      if (newTiles.length == 9) {
        val shuffledTiles = Random.shuffle(newTiles)
        client ! FinishedBoardGeneration(shuffledTiles)
      } else {
        context.become(awaitFeedBack(client, newTiles))
      }
    case FailedTileCreation(message) =>
      log.error(s"Failed board creation: $message")
      client ! FailedBoardGeneration(message)
    case any =>
      log.error(s"${this.getClass.getName} received unknown message : $any")
  }


  protected def generateBoard(userStats: UserStats, client: ActorRef)
                             (randomKindDrawer: (List[Int], List[QuestionKind], Int, Int) => List[QuestionKind],
                              randomTypeDrawer: (List[Int], List[DataType], Int, Int) => List[DataType]): Unit = {
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
      //val geoCount = normalizedCounts.getOrElse(Geolocation.toString, 0)

      //TODO : RE-ENABLE GEOLOCATION
      val selectedKinds = randomKindDrawer(List(tlCount, ordCount, mcCount),
        List(Timeline, Order, MultipleChoice), 27, 1)

      val pairsKindType: List[(QuestionKind, DataType)] = selectedKinds.groupBy(el => el).toList.flatMap {
        case (kind, kindList) =>
          val possTypes = possibleTypes(kind)
          val counts = possTypes.map(t => userStats.dataTypeCounts.getOrElse(t.name, 0))
          val selectedTypes =
            if (kind == Order) {
              randomTypeDrawer(counts, possTypes, kindList.length, orderingItemsNumber)
            } else {
              randomTypeDrawer(counts, possTypes, kindList.length, 1)
            }
          kindList.zip(selectedTypes)
      }

      generateWithKindTypePairs(List(), pairsKindType, client)

    } else {
      client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
    }

  }

  protected def generateWithKindTypePairs(alreadyFound: List[(QuestionKind, DataType, List[(String, String)])],
                                          pairsKindType: List[(QuestionKind, DataType)], client: ActorRef): Unit = pairsKindType match {
    case (cKind, cType) :: tail =>
      pairsKindType.partition { case (kind, dType) => (kind == cKind) && (dType == cType) } match {
        case (current, rest) =>
          val itemsStatsCollection = database[BSONCollection](MongoDatabaseService.itemsStatsCollection)
          val query = BSONDocument("userId" -> userId, "dataTypes" -> BSONDocument("$in" -> List(cType.name)), "readForStats" -> true)
          findSomeRandom[ItemStats](itemsStatsCollection, query, client) {
            listItemsStats =>
              cKind match {
                case Order =>
                  if (listItemsStats.length >= orderingItemsNumber * current.length) {
                    val groups = listItemsStats.groupBy(is => is.itemType).toList.map {
                      case (itemType, list) => list.map(itemStats => (itemStats.itemId, itemStats.itemType))
                    }
                    val randomBuckets = Random.shuffle(groups.flatMap(list => createBuckets[(String, String)](list, orderingItemsNumber)))
                    if (randomBuckets.length >= current.length) {
                      val newFound = current.zip(randomBuckets).map {
                        case ((kind, dType), v) => (kind, dType, v)
                      }
                      generateWithKindTypePairs(alreadyFound ++ newFound, rest, client)
                    } else {
                      client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
                    }
                  } else {
                    client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
                  }
                case _ =>
                  if (listItemsStats.length >= current.length) {
                    val newFound = current.zip(listItemsStats.map(is => (is.itemId, is.itemType))).map {
                      case ((kind, dType), v) => (kind, dType, List(v))
                    }
                    generateWithKindTypePairs(alreadyFound ++ newFound, rest, client)
                  } else {
                    client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
                  }
              }
          }
      }
    case _ =>
      sendOrders(client, alreadyFound)

  }

  protected def sendOrders(client: ActorRef, orders: List[(QuestionKind, DataType, List[(String, String)])]): Unit = {
    val tiles = generateTiles(orders, client)
    if (tiles.length > 9) {
      client ! FailedBoardGeneration(s"Too many tiles generated ! (${tiles.length})")
    }
    context.become(awaitFeedBack(client, List()))
    tiles.foreach {
      case (kind, choices) =>
        val worker = context.actorOf(TileGenerator.props(database))
        val req = CreateTile(userId, choices, kind)
        worker ! req
    }
  }

}
