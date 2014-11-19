package mongodb

import reactivemongo.bson.Macros.Options.{\/, UnionType, AllImplementations}
import reactivemongo.bson._
sealed trait Animal
case object Chicken extends Animal
case object Cow extends Animal

object AnimalFormat{
  implicit val animalf = Macros.handlerOpts[Animal,  AllImplementations]
}
import mongodb.AnimalFormat._
case class Possibility(id: Option[BSONObjectID] = None, text: String, imageUrl: String, animal: Animal)


object Possibility {
//  val DB_id = "_id"
//  val DB_text = "text"
//  val DB_imageUrl = "image_url"
//  implicit object PossibilityWriter extends BSONDocumentWriter[Possibility] {
//    def write(possibility: Possibility): BSONDocument = BSONDocument(
//      "_id" -> possibility.id.getOrElse(BSONObjectID.generate),
//      "text" -> possibility.text,
//      "image_url" -> possibility.imageUrl,
//      "animal" -> possibility.animal)
//  }
//  implicit object PossibilityReader extends BSONDocumentReader[Possibility] {
//    def read(doc: BSONDocument): Possibility = {
//      Possibility(
//        doc.getAs[BSONObjectID]("_id"),
//        doc.getAs[String]("text").get,
//        doc.getAs[String]("image_url").get,
//        doc.getAs[Animal]("animal").get)
//    }
//  }
  implicit val posibilityf = Macros.handler[Possibility]
}

