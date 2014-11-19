package mongodb

/**
 * Created by Aranir on 26/10/14.
 */
import reactivemongo.bson._
import org.joda.time.DateTime


case class Article(
                    id: Option[BSONObjectID],
                    title: String,
                    content: String,
                    publisher: String)

object Article {

  implicit object ArticleBSONReader extends BSONDocumentReader[Article] {
    def read(doc: BSONDocument): Article =
      Article(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("title").get,
        doc.getAs[String]("content").get,
        doc.getAs[String]("publisher").get)
  }

  implicit object ArticleBSONWriter extends BSONDocumentWriter[Article] {
    def write(article: Article): BSONDocument =
      BSONDocument(
        "_id" -> article.id.getOrElse(BSONObjectID.generate),
        "title" -> article.title,
        "content" -> article.content,
        "publisher" -> article.publisher)
  }

}