package crawler.FacebookConfig

import akka.util.Timeout

import org.json4s.DefaultFormats
import scala.concurrent.duration._
import scala.util.Properties._

/**
 * Created by roger on 05/03/15.
 */
case class FacebookAppConfig(app_id:String, app_secret:String)



object FacebookServiceConfig {
  implicit val formats = DefaultFormats
  val facebookStorageFolder = "facebookStorage"
  implicit val appID = envOrElse("FACEBOOK_APP_ID_REMINISCE_ME", "646896158757620")
  implicit val appSecret = envOrElse("FACEBOOK_APP_SECRET_REMINISCE_ME", "e480504686921c24d21e58ccf8f24925")
  val facebookHostAddress = "https://graph.facebook.com"
  val maxOffset = 10
  implicit val timeout = Timeout(10 seconds)

}
