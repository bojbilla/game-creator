package crawler.retrievedata

import akka.actor.Props
import crawler.common.GraphResponses.Page
import crawler.common.RetrieveEntitiesService.{FinishedRetrievingEntities, RetrieveEntities}
import crawler.retrievedata.RetrieverService.{Posts, TaggedPosts, LikedPages}
import crawler.retrievedata.retrievers.{RetrievePosts, RetrieveTaggedPosts, RetrieveLikedPages}
import org.joda.time.DateTime
import crawler.common.{GraphResponses, FBSimpleParameters}
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{BasicHttpCredentials, HttpResponse, HttpRequest}
import spray.httpx.encoding.Gzip
import com.github.nscala_time.time.Imports._

import scala.concurrent.Future
import scala.reflect.runtime.universe._
import spray.httpx.Json4sSupport
import org.json4s.DefaultFormats
import org.json4s._


/**
 * Created by roger on 15/11/14.
 */

object RetrieverService {
  case class LikedPages()
  case class TaggedPosts()
  case class UploadedPhotos()
  case class Posts()

  def props(params: FBSimpleParameters): Props =
    Props(new RetrieverService(params))
}
class RetrieverService(params: FBSimpleParameters) extends RetrieveData with Json4sSupport{
  val json4sFormats = DefaultFormats

  def receive = {
    case LikedPages() =>
      val retriever = context.actorOf(RetrieveLikedPages.props())
      val params1 = params.copy(minimalEntities = 0)
      retriever ! RetrieveEntities(params1)
      context.become(awaitResults())
    case TaggedPosts() =>
      val retriever = context.actorOf(RetrieveTaggedPosts.props())
      val params1 = params.copy(minimalEntities = 0, since = DateTime.now() - 10.year,until = DateTime.now())
      retriever ! RetrieveEntities(params1)
      context.become(awaitResults())
    case Posts() =>
      val retriever = context.actorOf(RetrievePosts.props())
      val params1 = params.copy(minimalEntities = 0, since = DateTime.now() - 10.year,until = DateTime.now())
      retriever ! RetrieveEntities(params1)
      context.become(awaitResults())
    case _ => log.error("RetrieverService received unexpected message")
  }

  def awaitResults(): Receive = {
    case fr @FinishedRetrievingEntities(entities) =>
      entities match {
        case pages:Vector[Page @unchecked] if fr.tpe =:= typeOf[Page] =>
          println(pages.map(p => p.id))
        case posts: Vector[GraphResponses.Post @unchecked] if fr.tpe =:= typeOf[GraphResponses.Post] =>
          println("we retrieved tagged posts")
        case _ => log.error("We did not receive any fb thingies")
      }
    case _ =>
      log.info("whats wrong")
      println("yea really")

  }

}
