package me.reminisce.service.gameboardgen

import akka.actor.{Actor, ActorLogging, ActorRef}
import me.reminisce.service.gameboardgen.BoardGenerator.FailedBoardGeneration
import me.reminisce.service.gameboardgen.GameGenerator.InitBoardCreation
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.Tile
import me.reminisce.service.stats.StatsDataTypes.DataType
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}
import reactivemongo.core.commands.Count

import scala.util.{Failure, Random, Success}

object BoardGenerator {

  case class FailedBoardGeneration(message: String)

  case class FinishedBoardGeneration(tiles: List[Tile])

}

abstract class BoardGenerator(database: DefaultDB, user_id: String) extends Actor with ActorLogging {

  def createGame(client: ActorRef): Unit

  def receive = {
    case InitBoardCreation() =>
      val client = sender()
      createGame(client)
    case any =>
      val client = sender()
      client ! FailedBoardGeneration(s"Inconsistent message passing.")
      log.error(s"Received any : $any")
  }

  def drawItemsAtRandomFromBags[T](bagSizes: List[Int], bagTypes: List[T], quantity: Int, drawnQuantity: Int = 1): List[T] = {
    if (quantity > 0) {
      // The thresholds define under which threshold a value belongs to a bag
      def findFirstLessOrEqual(bagThresholds: List[Int], bagTypes: List[T], generatedRand: Int): T = bagThresholds match {
        case Nil =>
          throw new IndexOutOfBoundsException
        case head :: tail =>
          if (generatedRand <= head)
            bagTypes.head
          else
            findFirstLessOrEqual(bagThresholds.tail, bagTypes.tail, generatedRand)
      }

      def formThresholds(acc: List[Int], sizes: List[Int]): List[Int] = sizes match {
        case Nil =>
          acc
        case head :: tail =>
          val newAcc = acc ++ List(acc.last + head)
          formThresholds(newAcc, tail)
      }

      val updatedBagSizes = bagSizes.map {
        s =>
          if (s >= drawnQuantity) {
            s
          } else {
            0
          }
      }

      val totalCount = updatedBagSizes.sum

      val bagThresholds = formThresholds(List(updatedBagSizes.head - 1), updatedBagSizes.tail)

      val selectedType = findFirstLessOrEqual(bagThresholds, bagTypes, Random.nextInt(totalCount))

      // we assume all the types are different
      val typePosition = bagTypes.indexOf(selectedType)

      val newSizes = (0 until updatedBagSizes.length).map {
        i =>
          if (i == typePosition) {
            updatedBagSizes(i) - drawnQuantity
          } else {
            updatedBagSizes(i)
          }
      }.toList

      selectedType :: drawItemsAtRandomFromBags[T](newSizes, bagTypes, quantity - 1)
    } else {
      List()
    }
  }

  def drawUniformelyFromBags[T](bagSizes: List[Int], bagTypes: List[T], quantity: Int): List[T] = {
    if (quantity > 0 && bagSizes.length > 0) {
      val picked = bagTypes.head
      val (newBagSizes, newBagTypes) =
        if (bagSizes.head > 1) {
          (bagSizes.tail :+ (bagSizes.head - 1), bagTypes.tail :+ bagTypes.head)
        } else {
          //it is equal then
          (bagSizes.tail, bagTypes.tail)
        }
      picked :: drawUniformelyFromBags[T](newBagSizes, newBagTypes, quantity - 1)
    } else {
      List()
    }
  }

  def createBuckets[T](list: List[T], bucketSize: Int): List[List[T]] = {
    if (list.size >= bucketSize) {
      val (left, right) = list.splitAt(bucketSize)
      left :: createBuckets(right, bucketSize)
    } else {
      List()
    }
  }

  def findOne[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (Option[T] => Unit))
                (implicit reader: BSONDocumentReader[T]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(selector).one[T].onComplete {
      case Success(opt) => f(opt)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"MongoDB error : ${e.getMessage}.")
    }
  }

  def findSome[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (List[T] => Unit))
                 (implicit reader: BSONDocumentReader[T]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(selector).cursor[T].collect[List]().onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"MongoDB error : ${e.getMessage}.")
    }
  }

  def findSomeRandom[T](db: DefaultDB,
                        collection: BSONCollection,
                        query: BSONDocument, quantity: Int, client: ActorRef)(f: (List[T] => Unit))
                       (implicit reader: BSONDocumentReader[T]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val futureCount = db.command(Count(collection.name, Some(query)))
    futureCount.flatMap { count =>
      val skip = if (count - quantity > 0) Random.nextInt(count - quantity) else 0
      collection.find(query).options(QueryOpts(skipN = skip)).cursor[T].collect[List](quantity)
    }.onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"MongoDB error : ${e.getMessage}.")
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
