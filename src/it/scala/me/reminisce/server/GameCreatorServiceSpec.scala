package me.reminisce.server

import java.util.concurrent.TimeUnit

import akka.actor._
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.database.StatsEntities.UserStats
import me.reminisce.fetching.config.GraphResponses.AccessTokenResponse
import me.reminisce.server.TestHelpers._
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, Formats}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Span}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpMethods, HttpRequest, HttpResponse, StatusCodes}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Properties._

class GameCreatorServiceSpec extends TestKit(ActorSystem("GameCreatorSpec")) with MongoEmbedDatabase
  with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll with ScalaFutures {


  val port = 27017
  val mongoProps: MongodProps = mongoStart(port = port)
  val driver: MongoDriver = new MongoDriver
  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )
  val testService = TestActorRef(new ServerServiceActor(s"localhost:$port", "mydb"))
  val testUserOpt: Option[AuthenticatedTestUser] = {
    val facebookAppId = envOrElse("FACEBOOK_APP_ID", "NONE")
    val facebookAppSecret = envOrElse("FACEBOOK_APP_SECRET", "NONE")

    // only way to get the message and stop the tests properly
    if (facebookAppId == "NONE") {
      throw new Exception("FACEBOOK_APP_ID environment variable is not set.")
    }
    if (facebookAppSecret == "NONE") {
      throw new Exception("FACEBOOK_APP_SECRET environment variable is not set.")
    }

    val facebookAPIPath = "https://graph.facebook.com/v2.7"

    val appAccessTokenPath = s"$facebookAPIPath/oauth/access_token?client_id=$facebookAppId" +
      s"&client_secret=$facebookAppSecret&grant_type=client_credentials"
    val appAccessTokenFuture = pipelineRawJson(Get(appAccessTokenPath))
    val appAccessTokenResponse = Await.result(appAccessTokenFuture, Duration(10, TimeUnit.SECONDS))
    assert(appAccessTokenResponse.status == StatusCodes.OK)
    val jsonResponse = parse(appAccessTokenResponse.entity.asString)
    val accessTokenResponse = jsonResponse.extract[AccessTokenResponse]
    val appAccessToken = accessTokenResponse.access_token
    assert(appAccessToken != "")

    val testUsersListPath = s"$facebookAPIPath/$facebookAppId/accounts/test-users?access_token=$appAccessToken"
    val testUsersListFuture = pipelineRawJson(Get(testUsersListPath))
    val testUsersListResponse = Await.result(testUsersListFuture, Duration(10, TimeUnit.SECONDS))
    assert(testUsersListResponse.status == StatusCodes.OK)
    val jsonTestUsersList = parse(testUsersListResponse.entity.data.asString)
    val testUserList = jsonTestUsersList.extract[TestUserList].data
    assert(testUserList.nonEmpty)

    val authenticatedUsers = testUserList.filter(_.access_token.isDefined).map(testUser =>
      AuthenticatedTestUser(testUser.id, testUser.access_token.get))
    assert(authenticatedUsers.nonEmpty)

    authenticatedUsers.headOption
  }

  implicit def json4sFormats: Formats = DefaultFormats

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    mongoStop(mongoProps)
    driver.system.terminate()
    system.terminate()
  }


  "GameboardService" must {

    "say that the token is invalid." in {
      val fetchRequest = HttpRequest(uri = "/fetchData?user_id=100007638947117&access_token=XXX")
      assert(fetchRequest.method == HttpMethods.GET)
      testService ! fetchRequest

      val responseOpt = Option(receiveOne(Duration(2, TimeUnit.SECONDS)))
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

    "successfully fetch data." in {

      def checkFinished(fetchRequest: HttpRequest, attempts: Int): Unit = {
        testService ! fetchRequest
        val finishedFetchingOpt = Option(receiveOne(Duration(2, TimeUnit.SECONDS)))

        finishedFetchingOpt match {
          case Some(finishedFetching) =>
            val done = finishedFetching.isInstanceOf[HttpResponse] &&
              finishedFetching.asInstanceOf[HttpResponse].status == StatusCodes.OK
            if (!done) {
              if (attempts < 20) {
                Thread.sleep(500)
                checkFinished(fetchRequest, attempts + 1)
              } else {
                fail("Did not finish fetching in time.")
              }
            }
          case None =>
            if (attempts < 20) {
              Thread.sleep(500)
              checkFinished(fetchRequest, attempts + 1)
            } else {
              fail("Did not finish fetching in time.")
            }
        }
      }

      testUserOpt match {
        case Some(testUser) =>
          val fetchRequest = Get(s"/fetchData?user_id=${testUser.id}&access_token=${testUser.accessToken}")
          assert(fetchRequest.method == HttpMethods.GET)
          testService ! fetchRequest

          val initialFetchResponseOpt = Option(receiveOne(Duration(2, TimeUnit.SECONDS)))
          initialFetchResponseOpt match {
            case Some(initialFetchResponse) =>
              assert(initialFetchResponse.isInstanceOf[HttpResponse])

              val initialFetchHttpResponse = initialFetchResponse.asInstanceOf[HttpResponse]
              assert(initialFetchHttpResponse.status == StatusCodes.OK)
              val initFetchJsonAnswer = parse(initialFetchHttpResponse.entity.data.asString)
              val initFetchMessage = initFetchJsonAnswer.extract[SimpleMessageFormat]
              assert(initFetchMessage.message == s"Fetching Data for ${testUser.id}")
            case None =>
              fail("InitialFetchResponse is not defined.")
          }

          testService ! fetchRequest

          val concurrentResponseOpt = Option(receiveOne(Duration(2, TimeUnit.SECONDS)))
          concurrentResponseOpt match {
            case Some(concurrentResponse) =>
              assert(concurrentResponse.isInstanceOf[HttpResponse])

              val concurrentHttpResponse = concurrentResponse.asInstanceOf[HttpResponse]
              assert(concurrentHttpResponse.status == StatusCodes.TooManyRequests)
              val concurrentGameCreatorJsonAnswer = parse(concurrentHttpResponse.entity.data.asString)
              val concurrentAnswer = concurrentGameCreatorJsonAnswer.extract[SimpleMessageFormat]
              assert(concurrentAnswer.message == s"Already fetching for user ${testUser.id}")
            case None =>
              fail("ConcurrentResponse is not defined.")
          }

          checkFinished(fetchRequest, 0)

        case None =>
          fail("TestUser is not defined.")
      }
    }

    "successfully generate a gameboard." in {
      testUserOpt match {
        case Some(testUser) =>
          val connection = driver.connection(s"localhost:$port" :: Nil)
          whenReady(connection.database("mydb"), timeout(Span(300, Millis)), interval(Span(30, Millis))) {
            db =>
              val collection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)
              val selector = BSONDocument("userId" -> testUser.id)
              val userStatsOpt = Await.result(collection.find(selector).one[UserStats], Duration(10, TimeUnit.SECONDS))

              userStatsOpt match {
                case Some(userStats) =>
                  val gameboardRequest = Get(s"/gameboard?user_id=${testUser.id}&access_token=${testUser.accessToken}&strategy=random")

                  testService ! gameboardRequest

                  val gameboardAnswerOpt = Option(receiveOne(Duration(10, TimeUnit.SECONDS)))
                  gameboardAnswerOpt match {
                    case Some(gameboardAnswer) =>
                      assert(gameboardAnswer.isInstanceOf[HttpResponse])
                      val gameboardHttpResponse = gameboardAnswer.asInstanceOf[HttpResponse]
                      assert(gameboardHttpResponse.status == StatusCodes.OK)
                    case None =>
                      fail("Did not receive a successful gameboard.")
                  }

                case None =>
                  cancel("Unable to find UserStats, the test user may have been wiped from its posts. " +
                    "It may also be that the fetching failed, please make sure that other tests pass.")

              }
          }
        case None =>
          fail("TestUser is not defined.")
      }
    }

    "successfully delete data for the user." in {
      testUserOpt match {
        case Some(testUser) =>
          val deleteRequest = Delete(s"/removeUser?user_id=${testUser.id}")
          testService ! deleteRequest

          val feedbackOpt = Option(receiveOne(Duration(10, TimeUnit.SECONDS)))
          feedbackOpt match {
            case Some(feedback) =>
              assert(feedback.isInstanceOf[HttpResponse])
              val feedbackHttpResponse = feedback.asInstanceOf[HttpResponse]
              assert(feedbackHttpResponse.status == StatusCodes.OK)
            case None =>
              fail("Did not receive feedback.")
          }
        case None =>
          fail("TestUser is not defined.")
      }
    }

    "successfully drop database." in {
      val deleteRequest = Delete(s"/dropDatabase")
      testService ! deleteRequest

      val feedbackOpt = Option(receiveOne(Duration(10, TimeUnit.SECONDS)))
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
}

object TestHelpers {

  case class SimpleMessageFormat(message: String)

  case class AuthenticatedTestUser(id: String, accessToken: String)

  case class TestUser(id: String, login_url: String, access_token: Option[String])

  case class Cursors(before: String, after: String)

  case class Paging(cursors: Cursors)

  case class TestUserList(data: List[TestUser], paging: Paging)

}