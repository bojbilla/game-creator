package me.reminisce.gameboard.board

import akka.actor.{Actor, ActorLogging, ActorRef}
import me.reminisce.gameboard.board.BoardGenerator.FailedBoardGeneration
import me.reminisce.gameboard.board.GameGenerator.InitBoardCreation
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.Tile
import me.reminisce.analysis.DataTypes.DataType
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}
import reactivemongo.core.commands.Count

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

/**
  * Useful methods for creating a gameboard and case classes for message passing
  */
object BoardGenerator {

  case class FailedBoardGeneration(message: String)

  case class FinishedBoardGeneration(tiles: List[Tile], strategy: String)

  /**
    * Draws items from bags at random. A bag is defined by its number of items and its content, we assume that bag i
    * contains bagSizes[i] items which all are bagContents[i]. This method is useful, for instance, to select question
    * kinds, if we know that we can build 10 MultipleChoice questions and 11 Timeline questions we can use this method
    * to select 10 kinds at random :
    * {{
    * scala> val selectedItems = drawItemsAtRandomFromBags[QuestionKind](List(10, 11), List(MultipleChoice, Timeline), 10)
    * }}
    * @param bagSizes sizes of the bags
    * @param bagContents content of each bag
    * @param quantity number of draws to perform
    * @param drawnQuantity number of objects drawn for each draw, for instance, when generating order questions,
    *                      one will remove not only one but multiple items will be removed from the bag as an order
    *                      question requires more than one item
    * @tparam T type of the items
    * @return list of drawn items
    */
  def drawItemsAtRandomFromBags[T](bagSizes: List[Int], bagContents: List[T], quantity: Int, drawnQuantity: Int = 1): List[T] = {

    //Makes this method tail recursive
    @tailrec
    def tailRecDraw(bagSizes: List[Int], bagContents: List[T], quantity: Int, drawnQuantity: Int, acc: List[T] = Nil): List[T] = {
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
              val selectedType = findFirstLessOrEqual(bagThresholds, bagContents, Random.nextInt(totalCount))
              val typePosition = bagContents.indexOf(selectedType)

              val newSizes = updatedBagSizes.indices.map {
                i =>
                  if (i == typePosition) {
                    updatedBagSizes(i) - drawnQuantity
                  } else {
                    updatedBagSizes(i)
                  }
              }.toList

              tailRecDraw(newSizes, bagContents, quantity - 1, drawnQuantity, selectedType :: acc)
            case Nil =>
              acc
          }
        } else {
          acc
        }
      } else {
        acc
      }
    }

    tailRecDraw(bagSizes, bagContents, quantity, drawnQuantity)

  }

  /**
    * Determines in what bag the random value lies based on the bag thresholds (which are the number of remaining items
    * in the bags)
    * @param bagThresholds list of limiting values for the bags
    * @param bagContents content of each bag
    * @param generatedRand generated random value
    * @tparam T type of items
    * @return the drawn item
    */
  @tailrec
  private def findFirstLessOrEqual[T](bagThresholds: List[Int], bagContents: List[T], generatedRand: Int): T = bagThresholds match {
    case Nil =>
      throw new IndexOutOfBoundsException
    case head :: tail =>
      if (generatedRand <= head)
        bagContents match {
          case hd :: tl =>
            hd
          case Nil =>
            throw new IndexOutOfBoundsException
        }
      else
        findFirstLessOrEqual(bagThresholds.tail, bagContents.tail, generatedRand)
  }

  /**
    * Form thresholds determining limits for a random value to lie in one bag or the other based on the number of items
    * in each bag.
    * @param acc an accumulator which holds the previously found thresholds
    * @param sizes sizes of the differend bags
    * @return a list of thresholds
    */
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

  /**
    * Draws items from bags at using a "uniform" strategy: we pick once per bag in a round robin fashion.
    * A bag is defined by its number of items and its content, we assume that bag i contains bagSizes[i]
    * items which all are bagContents[i]. See [[me.reminisce.gameboard.board.BoardGenerator.drawItemsAtRandomFromBags]]
    * for an example as those two methods are used the same way.
    * @param bagSizes sizes of the bags
    * @param bagContents content of each bag
    * @param quantity number of draws to perform
    * @param drawnQuantity number of objects drawn for each draw, explanation for this is given in
    *                      [[me.reminisce.gameboard.board.BoardGenerator.drawItemsAtRandomFromBags]]
    * @tparam T type of the items
    * @return list of drawn items
    */
  def drawUniformlyFromBags[T](bagSizes: List[Int], bagContents: List[T], quantity: Int, drawnQuantity: Int = 1): List[T] = {

    //Makes this method tail recursive
    @tailrec
    def tailRecDraw(bagSizes: List[Int], bagContents: List[T], quantity: Int, drawnQuantity: Int, acc: List[T] = Nil): List[T] = {
      if (quantity > 0) {
        val prunedSizesTypes = bagSizes.zip(bagContents).filter { case (size, tpe) => size >= drawnQuantity }
        val prunedSizes = prunedSizesTypes.map { case (size, tpe) => size }
        val prunedTypes = prunedSizesTypes.map { case (size, tpe) => tpe }
        (for {
          picked <- prunedTypes.headOption
          sizesHead <- prunedSizes.headOption
        } yield {
          (sizesHead, picked)
        }) match {
          case Some((sizesHead, picked)) =>
            if (sizesHead > drawnQuantity) {
              val newBagSizes = prunedSizes.tail :+ (sizesHead - drawnQuantity)
              val newBagTypes = prunedTypes.tail :+ picked
              tailRecDraw(newBagSizes, newBagTypes, quantity - drawnQuantity, drawnQuantity, picked :: acc)
            } else {
              //it is equal then
              val newBagSizes = bagSizes.tail
              val newBagTypes = bagContents.tail
              tailRecDraw(newBagSizes, newBagTypes, quantity - drawnQuantity, drawnQuantity, picked :: acc)
            }
          case _ =>
            acc
        }
      } else {
        acc
      }
    }

    tailRecDraw(bagSizes, bagContents, quantity, drawnQuantity)
  }

  /**
    * Form buckets of equal size fro ma list of items (a bucket is a list of items)
    * @param list items to put into buckets
    * @param bucketSize desired size of buckets
    * @param acc already formed buckets
    * @tparam T type of items in the buckets
    * @return a list of buckets
    */
  @tailrec
  def createBuckets[T](list: List[T], bucketSize: Int, acc: List[List[T]] = Nil): List[List[T]] = {
    if (list.size >= bucketSize) {
      list.splitAt(bucketSize) match {
        case (left, right) => createBuckets(right, bucketSize, left :: acc)
      }
    } else {
      acc
    }
  }

}

/**
  * Abstract board generating class
  * @param database database in which the data is stored
  * @param userId user for which the board is generated
  */
abstract class BoardGenerator(database: DefaultDB, userId: String) extends Actor with ActorLogging {

  /**
    * Implements the logic of actually creating a board
    * @param client the board requester
    */
  def createGame(client: ActorRef): Unit

  /**
    * This actor's entry point, handles the InitBoardCreation() message which triggers the board creation
    * @return Nothing
    */
  def receive = {
    case InitBoardCreation() =>
      val client = sender()
      createGame(client)
    case any =>
      val client = sender()
      client ! FailedBoardGeneration(s"Inconsistent message passing.")
      log.error(s"Received any : $any")
  }


  /**
    * Tries to find one item in collection which matches selector and applies the handler function f on it
    * @param collection collection to search in
    * @param selector selector to match
    * @param client original requester
    * @param f handles the returned value
    * @param reader implicit deserializer
    * @tparam T type of data to retrieve
    */
  protected def findOne[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (Option[T] => Unit))
                          (implicit reader: BSONDocumentReader[T]): Unit = {
    collection.find(selector).one[T].onComplete {
      case Success(opt) => f(opt)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"MongoDB error : ${e.getMessage}.")
      case any =>
        client ! FailedBoardGeneration(s"Unknown database error: $any.")
    }
  }

  /**
    * Tries to get as much data as possible from collection matching selector and then applies handler function f on it
    * @param collection collection to search in
    * @param selector selector to match
    * @param client original requester
    * @param f handles the returned values
    * @param reader implicit deserializer
    * @tparam T type of the data to retrieve
    */
  protected def findSome[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (List[T] => Unit))
                           (implicit reader: BSONDocumentReader[T]): Unit = {
    collection.find(selector).cursor[T]().collect[List]().onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"MongoDB error : ${e.getMessage}.")
      case any =>
        client ! FailedBoardGeneration(s"Unknown database error: $any.")
    }
  }

  /**
    * Same as [[me.reminisce.gameboard.board.BoardGenerator.findSome]] but retrieves a fixed number of items and asks
    * the database to skip a random number of results
    * @param db database t osearch in
    * @param collection collection so search in
    * @param query query to match
    * @param quantity number of items to retrieve
    * @param client original requester
    * @param f handle the results
    * @param reader implicit deserializer
    * @tparam T type of the retrieved items
    */
  protected def findSomeRandom[T](db: DefaultDB,
                                  collection: BSONCollection,
                                  query: BSONDocument, quantity: Int, client: ActorRef)(f: (List[T] => Unit))
                                 (implicit reader: BSONDocumentReader[T]): Unit = {
    val futureCount = collection.count(Some(query))
    futureCount.flatMap { count =>
      val skip = if (count - quantity > 0) Random.nextInt(count - quantity) else 0
      collection.find(query).options(QueryOpts(skipN = skip)).cursor[T]().collect[List](quantity)
    }.onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"MongoDB error : ${e.getMessage}.")
      case any =>
        client ! FailedBoardGeneration(s"Unknown database error: $any.")
    }
  }

  /**
    * Same as [[me.reminisce.gameboard.board.BoardGenerator.findSome]] except that it shuffles the results
    * @param collection collection to search in
    * @param selector selector to match
    * @param client original requester
    * @param f handles the returned values
    * @param reader implicit deserializer
    * @tparam T type of the data to retrieve
    */
  protected def findSomeRandom[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (List[T] => Unit))
                                 (implicit reader: BSONDocumentReader[T]): Unit = {
    collection.find(selector).cursor[T]().collect[List]().onComplete {
      case Success(list) => f(Random.shuffle(list))
      case Failure(e) =>
        client ! FailedBoardGeneration(s"MongoDB error : ${e.getMessage}.")
      case any =>
        client ! FailedBoardGeneration(s"Unknown database error: $any.")
    }
  }

  /**
    * Generates the tiles needed for a gameboard from a list of questions. A tile is a list of 3 questions, a question
    * is defined by the tuple (QuestionKind, DataType, List(itemId, itemType)). The list says what items will be used to
    * generate the question. Those tuples will be then sent to the appropriate question generator to generate an actual
    * question.
    * This functions attempts to generate tiles of questions of the same type.
    * @param generatedTuples questions generated, as tuples
    * @param client original requester
    * @return the list of tiles definitions
    */
  protected def generateTiles(generatedTuples: List[(QuestionKind, DataType, List[(String, String)])], client: ActorRef):
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
