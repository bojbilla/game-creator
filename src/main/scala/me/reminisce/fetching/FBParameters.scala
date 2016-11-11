package me.reminisce.fetching

import com.github.nscala_time.time.Imports._

/**
  * Parameters of a request to facebook graph API
  *
  * @param userId          user for which we want data
  * @param access_token    access_token for the user
  * @param query           the actual query
  * @param since           from which we want entities
  * @param until           until when we want entities
  */
case class FBParameters(userId: Option[String] = None,
                        access_token: Option[String] = None,
                        query: Option[String] = None,
                        since: DateTime = DateTime.now - 1.year,
                        until: DateTime = DateTime.now) {
  /**
    * Gets the since parameter
    *
    * @return since parameter as seconds
    */
  def getSince = {
    (since.getMillis / 1000).toString
  }

  /**
    * Gets the until parameter
    *
    * @return until parameter as seconds
    */
  def getUntil = {
    (until.getMillis / 1000).toString
  }
}
