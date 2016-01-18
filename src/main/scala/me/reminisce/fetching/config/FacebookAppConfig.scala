package me.reminisce.fetching.config

import akka.util.Timeout
import org.json4s.DefaultFormats

import scala.concurrent.duration._

case class FacebookAppConfig(appId: String, appSecret: String)


object FacebookServiceConfig {
  implicit val formats = DefaultFormats
  val facebookStorageFolder = "facebookStorage"
  val facebookHostAddress = "https://graph.facebook.com"
  val apiVersion = "v2.2"
  val maxOffset = 10
  implicit val timeout = Timeout(10.seconds)
}
