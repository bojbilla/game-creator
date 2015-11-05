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

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
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
    if (quantity > 0 && bagSizes.sum > 0) {
      val updatedBagSizes = bagSizes.map {
        s =>
          if (s >= drawnQuantity) {
            s
          } else {
            0
          }
      }
      val totalCount = updatedBagSizes.sum

      if (totalCount > 0) {
        updatedBagSizes match {
          case head :: tail =>
            val bagThresholds = formThresholds(List(head - 1), tail)
            val selectedType = findFirstLessOrEqual(bagThresholds, bagTypes, Random.nextInt(totalCount))
            val typePosition = bagTypes.indexOf(selectedType)

            val newSizes = updatedBagSizes.indices.map {
              i =>
                if (i == typePosition) {
                  updatedBagSizes(i) - drawnQuantity
                } else {
                  updatedBagSizes(i)
                }
            }.toList

            selectedType :: drawItemsAtRandomFromBags[T](newSizes, bagTypes, quantity - 1)
          case Nil =>
            List()
        }
      } else {
        List()
      }
    } else {
      List()
    }
  }

  @tailrec
  private def findFirstLessOrEqual[T](bagThresholds: List[Int], bagTypes: List[T], generatedRand: Int): T = bagThresholds match {
    case Nil =>
      throw new IndexOutOfBoundsException
    case head :: tail =>
      if (generatedRand <= head)
        bagTypes match {
          case hd :: tl =>
            hd
          case Nil =>
            throw new IndexOutOfBoundsException
        }
      else
        findFirstLessOrEqual(bagThresholds.tail, bagTypes.tail, generatedRand)
  }

  // The thresholds define under which threshold a value belongs to a bag
  @tailrec
  private def formThresholds(acc: List[Int], sizes: List[Int]): List[Int] = sizes match {
    case Nil =>
      acc
    case head :: tail =>
      val newAcc = acc.lastOption match {
        case Some(last) =>
          acc ++ List(last + head)
        case None =>
          acc ++ List(head)
      }
      formThresholds(newAcc, tail)
  }

  def drawUniformlyFromBags[T](bagSizes: List[Int], bagTypes: List[T], quantity: Int, drawnQuantity: Int = 1): List[T] = {
    if (quantity > 0) {
      (for {
        picked <- bagTypes.headOption
        sizesHead <- bagSizes.headOption
      } yield {
        if (sizesHead > 1) {
          (bagSizes.tail :+ (sizesHead - 1), bagTypes.tail :+ picked)
        } else {
          //it is equal then
          (bagSizes.tail, bagTypes.tail)
        }
      } match {
        case (newBagSizes, newBagTypes) =>
          picked :: drawUniformlyFromBags[T](newBagSizes, newBagTypes, quantity - drawnQuantity)
      }).getOrElse(Nil)
    } else {
      List()
    }
  }

  def createBuckets[T](list: List[T], bucketSize: Int): List[List[T]] = {
    if (list.size >= bucketSize) {
      list.splitAt(bucketSize) match {
        case (left, right) => left :: createBuckets(right, bucketSize)
      }
    } else {
      List()
    }
  }

  def findOne[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (Option[T] => Unit))
                (implicit reader: BSONDocumentReader[T]): Unit = {
    collection.find(selector).one[T].onComplete {
      case Success(opt) => f(opt)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"MongoDB error : ${e.getMessage}.")
    }
  }

  def findSome[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (List[T] => Unit))
                 (implicit reader: BSONDocumentReader[T]): Unit = {
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
        val groups = generatedTuples.groupBy { case (qKind, dType, idTypeList) => qKind }.toList
        val largeGroups = groups.filter { case (qKind, tupGroups) => tupGroups.length >= 3 }
        largeGroups match {
          case (qKind, tupGroups) :: tail =>
            val selectedQuestions = tupGroups.take(3)
            selectedQuestions.headOption match {
              case Some((kind, dType, idTypeList)) =>
                (kind, selectedQuestions) :: generateTiles(generatedTuples.filterNot(selectedQuestions.toSet), client)
              case None =>
                generateTiles(generatedTuples.filterNot(selectedQuestions.toSet), client)
            }
          case Nil =>
            generatedTuples.splitAt(3) match {
              case (left, right) =>
                (Misc, left) :: generateTiles(right, client)
            }
        }
      case Nil =>
        List()
    }
  }
}
