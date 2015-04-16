package me.reminisce.mock.data

import com.github.nscala_time.time.Imports._
import me.reminisce.entities.Entities
import me.reminisce.entities.Entities.{Question, TimelineQuestion}

import scala.util.Random

/**
 * Created by roger on 10/11/14.
 */
class TimelineMock {
  private val random = new Random()
  private val range = 4
  private val user_id = "1234513"
  private val question1 = Question("When did you share this post", Some(List("finally graduated!")),
    Some("https://fbcdn-sphotos-c-a.akamaihd.net/hphotos-ak-xfp1/v/t1.0-9/10320518_633371713422" +
      "361_8865470906024206323_n.jpg?oh=c6592cf487855b03339ea57d4a0b1e0a&oe=54EC7EBE&__gda__=14" +
      "27920695_cdbd7d718a148b20785c2a35670714ba"))
  private val date1 = DateTime.lastMonth - random.nextInt(10).days
  private val min1 = random.nextInt(20)
  private val max1 = 20 - min1
  val timeline1 = TimelineQuestion("43253634", user_id, question1, min1, max1, range, date1)

  private val question2 = Question("When is your friends birthday?", Some(List("10153179507419968")), None)
  private val date2 = new DateTime().withDate(1982, 5, 4)
  private val min2 = random.nextInt(20)
  private val max2 = 20 - min2
  val timeline2 = TimelineQuestion("98766512", user_id, question2, min2, max2, range, date2)

  private val image1 = "http://daddu.net/wp-content/uploads/2011/10/3072689116_146665230f_z.jpg"
  private val image2 = "https://31.media.tumblr.com/421761f8ae36c7949f3978162a6e1b6e/tumblr_" +
    "inline_nd1ay77IZm1ro2d0d.jpg"
  private val image: String = if (random.nextBoolean()) image1 else image2


  private val question3 = Question("When did you share this post", Some(List("Look at that cute cat!")),
    Some(image))

  private val date3 = DateTime.lastMonth - random.nextInt(20).days
  private val min3 = random.nextInt(20)
  private val max3 = 20 - min2
  val timeline3 = TimelineQuestion("23239482938", user_id, question3, min3, max3, range, date3)


}
