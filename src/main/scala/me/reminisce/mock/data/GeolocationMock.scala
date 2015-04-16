package me.reminisce.mock.data

import me.reminisce.entities.Entities
import me.reminisce.entities.Entities.{CoordinatesQuestion, Location, Question}

/**
 * Created by roger on 10/11/14.
 */
class GeolocationMock {
  private val user_id = "1234513"
  private val question1 = Question("Where did you take this picture?", None, Some("http://t1.gstatic.com/images?q=tbn" +
    ":ANd9GcQDXP4x3jmst5VxRbO-yT8-NOeAhz-MrCER_D591Fj20Ps23wNdQg"))
  private val location1 = Location(48.858093, 2.294694)
  val geolocation1 = CoordinatesQuestion("1234789247", user_id, question1, location1)

  private val question2 = Question("Where were you when you wrote that post?", Some(List("Still working here")),
    Some("http://isic3.epfl.ch/scs-fallmeeting/images/EPFL_panorama1.jpg"))
  private val location2 = Location(46.522052, 6.566304)
  val geolocation2 = CoordinatesQuestion("487982734", user_id, question2, location2)

  private val question3 = Question("Where did you take this picture?", None,
    Some("http://www.wandersite.ch/Vierwaldstaettersee_panoramabahn.jpg"))
  private val location3 = Location(47.05032, 8.311831)
  val geolocation3 = CoordinatesQuestion("986786223", user_id, question3, location3)


}
