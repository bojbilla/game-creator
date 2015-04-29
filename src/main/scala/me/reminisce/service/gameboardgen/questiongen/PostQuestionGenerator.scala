package me.reminisce.service.gameboardgen.questiongen

import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.Count

import scala.concurrent.Future
import scala.util.Random

abstract class PostQuestionGenerator(db: DefaultDB) extends QuestionGenerator {
  val collection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)


  def getDocument(db: DefaultDB,
                  collection: BSONCollection,
                  query: BSONDocument): Future[Option[FBPost]] = {
    val futureCount = db.command(Count(collection.name, Some(query)))
    futureCount.flatMap { count =>
      val skip = Random.nextInt(count)
      collection.find(query).
        options(QueryOpts(skipN = skip)).one[FBPost]

    }
  }
}
