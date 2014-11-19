import org.joda.time.DateTime
import play.api.libs.iteratee.Iteratee

import reactivemongo.api._
import reactivemongo.bson._

//import org.joda.time.DateTime
import scala.concurrent.Future

import reactivemongo.api.gridfs.GridFS
import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._
import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._


import scala.concurrent.ExecutionContext.Implicits.global
import mongodb.Possibility._
import mongodb.{Chicken, Possibility, Article}
import mongodb.Article._
import mongodb.AnimalFormat._

object QuickStart {
//  def main(args: Array[String]) {
//    println("Hello, world!")
//    connect()
//    println("done")
//  }


  def connect() {
    import reactivemongo.api._
    import scala.concurrent.ExecutionContext.Implicits.global

    // gets an instance of the driver
    // (creates an actor system)
    val driver = new MongoDriver
    val connection: MongoConnection = driver.connection(List("localhost"))

    // Gets a reference to the database "plugin"
    val db: DefaultDB = connection("mydb")
    val pos = Possibility(None, "true that", "www.something.com", Chicken)
    val article = Article(None, "hello", "something", "me")
    val collection = db[BSONCollection]("testingCase")

//    collection.insert(article)
    collection.insert(pos)
//    val query = BSONDocument("title" -> BSONDocument("$exists" -> "1"))
    // select only the fields 'lastName' and '_id'
    val query = BSONDocument()
    val filter = BSONDocument()

    val cursor = collection.find(query).cursor[Possibility]

    val futureList = cursor.collect[List]()
    futureList.map { list =>
      list.foreach(println)
    } recover {
      case _ => println("Something went terribly wrong")
    }
  }
}