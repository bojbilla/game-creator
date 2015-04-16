package me.reminisce.service

/**
 * Created by roger on 17/11/14.
 */

object GameRequest {
  val fbPageType = "FBPageType"
  val fbPostType = "FBPostType"
  val fbTaggedPostType = "FBTaggedPostType"
}

case class GameRequest(user_id: String,
                       access_token: String,
                       return_address: String,
                       crawler_address: String,
                       retrieveType: Option[String] = None) {


}
