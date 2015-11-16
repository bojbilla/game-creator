package me.reminisce.service.gameboardgen

import akka.actor.ActorRef
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.StatsEntities.{ItemStats, UserStats}
import me.reminisce.service.gameboardgen.BoardGenerator.FailedBoardGeneration
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.stats.StatsDataTypes._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONArray, BSONDocument}

import scala.util.Random

object FullRandomBoardGenerator

class FullRandomBoardGenerator(database: DefaultDB, userId: String) extends RandomBoardGenerator(database, userId) {

  def createGame(client: ActorRef): Unit = {
    val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    val selector = BSONDocument("userId" -> userId)
    findOne[UserStats](userCollection, selector, client) {
      case Some(userStats) =>
        generateBoard(userStats, client)(drawItemsAtRandomFromBags[QuestionKind], drawItemsAtRandomFromBags[DataType])
      case None =>
        generateBoard(client)
    }
  }

  private def generateBoard(client: ActorRef): Unit = {
    // As order cannot be generated only from one question, we exclude the types that only lead to this kind of question
    // PostWhoLiked is also excluded as it cannot be generated without UserStats (even though this type should not be
    // marked as available on an item if no UserStats was generated)
    //TODO : RE-ENABLE GEOLOCATION
    //For now geolocation questions are disabled
    val excluded = (PostWhoLiked :: possibleTypes(Order).filter(t => possibleKind(t).size == 1)
      ::: possibleTypes(Geolocation).filter(t => possibleKind(t).size == 1)).map(_.name)
    val selector = BSONDocument("userId" -> userId,
      "$or" -> BSONArray(
        BSONDocument("dataCount" -> BSONDocument("$gt" -> excluded.size)),
        BSONDocument("dataTypes" -> BSONDocument("$nin" -> excluded))))
    val itemsStatsCollection = database[BSONCollection](MongoDatabaseService.itemsStatsCollection)
    findSomeRandom[ItemStats](itemsStatsCollection, selector, client) {
      itemsStatsList =>
        if (itemsStatsList.length < 27) {
          client ! FailedBoardGeneration(s"Failed to generate board for user $userId : not enough data.")
        } else {
          val shuffledList = Random.shuffle(itemsStatsList).take(27)
          val tuples = shuffledList.flatMap {
            is =>
              val okTypes = is.dataTypes.filterNot(t => excluded.contains(t)).map(stringToType)
              for {
                chosenType <- Random.shuffle(okTypes).headOption
                chosenKind <- Random.shuffle(possibleKind(chosenType).filterNot(k => k == Order)).headOption
              } yield (chosenKind, chosenType, List((is.itemId, is.itemType)))
          }

          sendOrders(client, tuples)
        }
    }
  }

}
