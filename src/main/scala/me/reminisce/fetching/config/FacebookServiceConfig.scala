package me.reminisce.fetching.config

import akka.util.Timeout
import org.json4s.DefaultFormats

import scala.concurrent.duration._

/**
  * Contains the configuration for the service contacting Facebook.
  */
object FacebookServiceConfig {
  implicit val formats = DefaultFormats
  val facebookStorageFolder = "facebookStorage"
  val facebookHostAddress = "https://graph.facebook.com"
  val apiVersion = "v2.7"
  val maxOffset = 10
  implicit val timeout = Timeout(10.seconds)
}
