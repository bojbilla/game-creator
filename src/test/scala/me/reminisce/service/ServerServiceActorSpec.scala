package me.reminisce.service

import java.util.concurrent.TimeUnit

import akka.testkit.TestActorRef
import me.reminisce.database.DatabaseTester
import me.reminisce.server.ServerServiceActor
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, Formats}
import org.scalatest.DoNotDiscover
import spray.client.pipelining._
import spray.http.{HttpMethods, HttpRequest, HttpResponse, StatusCodes}

import scala.concurrent.duration.Duration

@DoNotDiscover
class ServerServiceActorSpec extends DatabaseTester("ServerServiceActorSpec") {

  case class SimpleMessageFormat(message: String)

  val testService = TestActorRef[ServerServiceActor]

  implicit def json4sFormats: Formats = DefaultFormats

  "ServerServiceActor" must {
    "try to fetch." in {
      val fetchRequest = new HttpRequest(uri = "/fetchData?user_id=XXX&access_token=XXX")
      assert(fetchRequest.method == HttpMethods.GET)
      testService ! fetchRequest
      val responseOpt = Option(receiveOne(Duration(10, TimeUnit.SECONDS)))
      responseOpt match {
        case Some(response) =>
          assert(response.isInstanceOf[HttpResponse])

          val httpResponse = response.asInstanceOf[HttpResponse]
          assert(httpResponse.status == StatusCodes.Unauthorized)
          val json = parse(httpResponse.entity.data.asString)
          val message = json.extract[SimpleMessageFormat]
          assert(message.message == "The specified token is invalid.")
        case None =>
          fail("Response is not defined.")
      }
    }

    "try to generate a gameboard." in {
      val gameboardRequest = Get(s"/gameboard?user_id=XXX&access_token=XXX&strategy=random")

      testService ! gameboardRequest

      val gameboardAnswerOpt = Option(receiveOne(Duration(10, TimeUnit.SECONDS)))
      gameboardAnswerOpt match {
        case Some(gameboardAnswer) =>
          assert(gameboardAnswer.isInstanceOf[HttpResponse])
          val gameboardHttpResponse = gameboardAnswer.asInstanceOf[HttpResponse]
          assert(gameboardHttpResponse.status == StatusCodes.InternalServerError)
        case None =>
          fail("Did not receive an answer.")
      }
    }

    "delete user." in {
      val deleteRequest = Delete(s"/removeUser?user_id=XXX")
      testService ! deleteRequest

      val feedbackOpt = Option(receiveOne(Duration(10, TimeUnit.SECONDS)))
      checkFeedBack(feedbackOpt)
    }

    "drop database." in {
      val deleteRequest = Delete(s"/dropDatabase")
      testService ! deleteRequest

      val feedbackOpt = Option(receiveOne(Duration(10, TimeUnit.SECONDS)))
      checkFeedBack(feedbackOpt)
    }

    "get info." in {
      val deleteRequest = Get(s"/info")
      testService ! deleteRequest

      val feedbackOpt = Option(receiveOne(Duration(10, TimeUnit.SECONDS)))
      checkFeedBack(feedbackOpt)
    }
  }

  private def checkFeedBack(feedbackOpt: Option[AnyRef]): Unit = {
    feedbackOpt match {
      case Some(feedback) =>
        assert(feedback.isInstanceOf[HttpResponse])
        val feedbackHttpResponse = feedback.asInstanceOf[HttpResponse]
        assert(feedbackHttpResponse.status == StatusCodes.OK)
      case None =>
        fail("Did not receive feedback.")
    }
  }
}
