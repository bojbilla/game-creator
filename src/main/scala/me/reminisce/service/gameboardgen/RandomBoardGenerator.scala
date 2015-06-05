package me.reminisce.service.gameboardgen

import akka.actor.ActorRef
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{ItemStats, UserStats}
import me.reminisce.service.gameboardgen.BoardGenerator.{FailedBoardGeneration, FinishedBoardGeneration}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.Tile
import me.reminisce.service.gameboardgen.tilegen.TileGenerator
import me.reminisce.service.gameboardgen.tilegen.TileGenerator.{CreateTile, FailedTileCreation, FinishedTileCreation}
import me.reminisce.service.stats.StatsDataTypes._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONArray, BSONDocument}

import scala.util.Random

object RandomBoardGenerator {

}

class RandomBoardGenerator(database: DefaultDB, userId: String) extends BoardGenerator(database, userId) {

  var tiles: List[Tile] = List()

  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    val selector = BSONDocument("userId" -> userId)
    findOne[UserStats](userCollection, selector, client) {
      case Some(userStats) =>
        generateBoard(userStats, client)
      case None =>
        generateBoard(client)
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
    // An order question is made of 4 items
    val normalizedCounts = userStats.questionCounts.map {
      case (k, v) =>
        if (k == Order.toString) {
          k -> v / 4
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

      val selectedKinds = drawItemsAtRandomFromBags[QuestionKind](List(tlCount, ordCount, mcCount, geoCount),
        List(Timeline, Order, MultipleChoice, Geolocation), 27)



      val pairsKindType: List[(QuestionKind, DataType)] = selectedKinds.groupBy(el => el).toList.flatMap {
        case (kind, kindList) =>
          val possTypes = possibleTypes(kind)
          val counts = possTypes.map(t => userStats.dataTypeCounts.getOrElse(t.name, 0))
          val selectedTypes =
            if (kind == Order) {
              drawItemsAtRandomFromBags[DataType](counts, possTypes, kindList.length, 4)
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
            if (listItemsStats.length >= 4 * current.length) {
              val groups = listItemsStats.groupBy(is => is.itemType).toList.map { case (itemType, list) => list.map(is => (is.itemId, is.itemType)) }
              val randomBuckets = Random.shuffle(groups.flatMap(list => createBuckets[(String, String)](list, 4)))
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

  def generateBoard(client: ActorRef): Unit = {
    // As order cannot be generated only from one question, we exclude the types that only lead to this kind of question
    // PostWhoLiked is also excluded as it cannot be generated without UserStats (even though this type should not be
    // marked as available on an item if no UserStats was generated)
    val excluded = (PostWhoLiked :: possibleTypes(Order).filter(t => possibleKind(t).size == 1)).map(_.name)
    val selector = BSONDocument("userId" -> userId,
      "$or" -> BSONArray(
        BSONDocument("dataCount" -> BSONDocument("$gt" -> excluded.size)),
        BSONDocument("dataTypes" -> BSONDocument("$nin" -> excluded))))
    val itemsStatsCollection = database[BSONCollection](MongoDatabaseService.itemsStatsCollection)
    findSome[ItemStats](itemsStatsCollection, selector, client) {
      itemsStatsList =>
        if (itemsStatsList.length < 27) {
          client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
        } else {
          val shuffledList = Random.shuffle(itemsStatsList).take(27)
          val tuples = shuffledList.map {
            is =>
              val okTypes = is.dataTypes.filterNot(t => excluded.contains(t)).map(stringToType)
              val chosenType = Random.shuffle(okTypes).head
              val chosenKind = Random.shuffle(possibleKind(chosenType).filterNot(k => k == Order)).head
              (chosenKind, chosenType, List((is.itemId, is.itemType)))
          }

          val tiles = generateTiles(tuples, client)
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
  }

  def generateTiles(generatedTuples: List[(QuestionKind, DataType, List[(String, String)])], client: ActorRef):
  List[(QuestionKind, List[(QuestionKind, DataType, List[(String, String)])])] = {
    if (generatedTuples.length > 27) {
      client ! FailedBoardGeneration(s"Too many tuples generated ${generatedTuples.length}")
    }
    generatedTuples match {
      case x :: xs =>
        val groups = generatedTuples.groupBy(el => el._1).toList
        val largeGroups = groups.filter(cpl => cpl._2.length >= 3)
        largeGroups match {
          case head :: tail =>
            val selectedQuestions = head._2.take(3)
            val kind = selectedQuestions.head._1
            (kind, selectedQuestions) :: generateTiles(generatedTuples.filterNot(selectedQuestions.toSet), client)
          case Nil =>
            val (left, right) = generatedTuples.splitAt(3)
            (Misc, left) :: generateTiles(right, client)
        }
      case Nil =>
        List()
    }
  }

}
