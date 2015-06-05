package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{OrderQuestion, SubjectWithId}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection

import scala.util.Random


object OrderByPageLikes {

  def props(database: DefaultDB): Props =
    Props(new OrderByPageLikes(database))
}

class OrderByPageLikes(db: DefaultDB) extends QuestionGenerator {
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
      fetchPages(pagesCollection, itemIds, client) {
        pageList =>
          if (pageList.length < 4) {
            client ! NotEnoughData(s"Not enough pages in list.")
          } else {
            val ordered = pageList.take(4).sortBy(_.likesNumber)
            val answer = (0 until 4).toList
            val subjectsWithId = Random.shuffle(ordered.zip(answer).map {
              case (page, id) => SubjectWithId(subjectFromPage(page), id)
            })
            val gameQuestion = OrderQuestion(userId, Order, ORDPageLikes, None, subjectsWithId, answer)
            client ! FinishedQuestionCreation(gameQuestion)
          }
      }
  }

}
