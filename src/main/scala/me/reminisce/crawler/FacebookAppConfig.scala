package me.reminisce.crawler

import akka.util.Timeout
import org.json4s.DefaultFormats

import scala.concurrent.duration._

/**
 * Created by roger on 05/03/15.
 */
case class FacebookAppConfig(app_id: String, app_secret: String)


object FacebookServiceConfig {
  implicit val formats = DefaultFormats
  val facebookStorageFolder = "facebookStorage"
  val facebookHostAddress = "https://graph.facebook.com"
  val apiVersion = "v2.2"
  val maxOffset = 10
  implicit val timeout = Timeout(10 seconds)
}
