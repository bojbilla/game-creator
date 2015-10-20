package me.reminisce.service

import java.util.concurrent.TimeUnit

import akka.actor._
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.UserStats
import me.reminisce.server.ServerServiceActor
import me.reminisce.service.TestHelpers._
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, Formats}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpMethods, HttpRequest, HttpResponse, StatusCodes}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Properties._

class GameCreatorServiceSpec extends TestKit(ActorSystem("GameCreatorSpec")) with MongoEmbedDatabase
with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit def json4sFormats: Formats = DefaultFormats

  var mongoProps: MongodProps = null
  var driver: MongoDriver = null

  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )

  val testService = TestActorRef[ServerServiceActor]

  var testUser: AuthenticatedTestUser = null

  override def beforeAll(): Unit = {
    val facebookAppId = envOrElse("FACEBOOK_APP_ID", "NONE")
    val facebookAppSecret = envOrElse("FACEBOOK_APP_SECRET", "NONE")

    // only way to get the message and stop the tests properly
    if (facebookAppId == "NONE") {
      throw new Exception("FACEBOOK_APP_ID environment variable is not set.")
    }
    if (facebookAppSecret == "NONE") {
      throw new Exception("FACEBOOK_APP_SECRET environment variable is not set.")
    }

    val facebookAPIPath = "https://graph.facebook.com/v2.2"

    val appAccessTokenPath = s"$facebookAPIPath/oauth/access_token?client_id=$facebookAppId" +
      s"&client_secret=$facebookAppSecret&grant_type=client_credentials"
    val appAccessTokenFuture = pipelineRawJson(Get(appAccessTokenPath))
    val appAccessTokenResponse = Await.result(appAccessTokenFuture, Duration(10, TimeUnit.SECONDS))
    assert(appAccessTokenResponse.status == StatusCodes.OK)
    val splitResponse = appAccessTokenResponse.entity.data.asString.split("=")
    assert(splitResponse.length > 1)
    val appAccessToken = splitResponse(1)
    assert(appAccessToken != "")

    val testUsersListPath = s"$facebookAPIPath/$facebookAppId/accounts/test-users?access_token=$appAccessToken"
    val testUsersListFuture = pipelineRawJson(Get(testUsersListPath))
    val testUsersListResponse = Await.result(testUsersListFuture, Duration(10, TimeUnit.SECONDS))
    assert(testUsersListResponse.status == StatusCodes.OK)
    val jsonTestUsersList = parse(testUsersListResponse.entity.data.asString)
    val testUserList = jsonTestUsersList.extract[TestUserList].data
    assert(testUserList.length > 0)

    val authenticatedUsers = testUserList.filter(_.access_token.isDefined).map(testUser =>
      AuthenticatedTestUser(testUser.id, testUser.access_token.get))
    assert(authenticatedUsers.length > 0)

    testUser = authenticatedUsers.head

    assert(testUser != null)

    mongoProps = mongoStart(port = 27017)
    driver = new MongoDriver

    assert(mongoProps != null)
    assert(driver != null)
  }

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    mongoStop(mongoProps)
    driver.system.shutdown()
    system.shutdown()
  }


  "GameboardService" must {

    "say that the token is invalid." in {
      val fetchRequest = new HttpRequest(uri = "/fetchData?user_id=100007638947117&access_token=XXX")
      assert(fetchRequest.method == HttpMethods.GET)
      testService ! fetchRequest

      val response = receiveOne(Duration(2, TimeUnit.SECONDS))
      assert(response != null)
      assert(response.isInstanceOf[HttpResponse])

      val httpResponse = response.asInstanceOf[HttpResponse]
      assert(httpResponse.status == StatusCodes.Unauthorized)
      val json = parse(httpResponse.entity.data.asString)
      val message = json.extract[SimpleMessageFormat]
      assert(message.message == "The specified token is invalid.")
    }

    "successfully fetch data." in {

      val fetchRequest = Get(s"/fetchData?user_id=${testUser.id}&access_token=${testUser.accessToken}")
      assert(fetchRequest.method == HttpMethods.GET)
      testService ! fetchRequest

      val initialFetchResponse = receiveOne(Duration(2, TimeUnit.SECONDS))
      assert(initialFetchResponse != null)
      assert(initialFetchResponse.isInstanceOf[HttpResponse])

      val initialFetchHttpResponse = initialFetchResponse.asInstanceOf[HttpResponse]
      assert(initialFetchHttpResponse.status == StatusCodes.OK)
      val initFetchJsonAnswer = parse(initialFetchHttpResponse.entity.data.asString)
      val initFetchMessage = initFetchJsonAnswer.extract[SimpleMessageFormat]
      assert(initFetchMessage.message == s"Fetching Data for ${testUser.id}")

      testService ! fetchRequest

      val concurrentResponse = receiveOne(Duration(2, TimeUnit.SECONDS))
      assert(concurrentResponse != null)
      assert(concurrentResponse.isInstanceOf[HttpResponse])

      val concurrentHttpResponse = concurrentResponse.asInstanceOf[HttpResponse]
      assert(concurrentHttpResponse.status == StatusCodes.TooManyRequests)
      val concurrentGameCreatorJsonAnswer = parse(concurrentHttpResponse.entity.data.asString)
      val concurrentAnswer = concurrentGameCreatorJsonAnswer.extract[SimpleMessageFormat]
      assert(concurrentAnswer.message == s"Already fetching for user ${testUser.id}")

      var finishedFetching = false

      while (!finishedFetching) {
        Thread sleep 500
        testService ! fetchRequest
        val resp = receiveOne(Duration(2, TimeUnit.SECONDS))
        finishedFetching = (resp != null) && (resp.asInstanceOf[HttpResponse].status == StatusCodes.OK)
      }

      assert(finishedFetching)
    }

    "successfully generate a gameboard." in {
      val connection = driver.connection("localhost:27017" :: Nil)
      val db = connection("mydb")
      val collection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)
      val selector = BSONDocument("userId" -> testUser.id)
      val userStats = Await.result(collection.find(selector).one[UserStats], Duration(10, TimeUnit.SECONDS))
      assert(userStats.isDefined)

      val gameboardRequest = Get(s"/gameboard?user_id=${testUser.id}&access_token=${testUser.accessToken}&strategy=random")

      testService ! gameboardRequest

      val gameboardAnswer = receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(gameboardAnswer != null)
      assert(gameboardAnswer.isInstanceOf[HttpResponse])

      val gameboardHttpResponse = gameboardAnswer.asInstanceOf[HttpResponse]
      assert(gameboardHttpResponse.status == StatusCodes.OK)
    }

    "successfully delete data for the user." in {
      val deleteRequest = Delete(s"/removeUser?user_id=${testUser.id}")
      testService ! deleteRequest

      val feedback = receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(feedback != null)
      assert(feedback.isInstanceOf[HttpResponse])

      val feedbackHttpResponse = feedback.asInstanceOf[HttpResponse]
      assert(feedbackHttpResponse.status == StatusCodes.OK)
    }

    "successfully drop database." in {
      val deleteRequest = Delete(s"/dropDatabase")
      testService ! deleteRequest

      val feedback = receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(feedback != null)
      assert(feedback.isInstanceOf[HttpResponse])

      val feedbackHttpResponse = feedback.asInstanceOf[HttpResponse]
      assert(feedbackHttpResponse.status == StatusCodes.OK)
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