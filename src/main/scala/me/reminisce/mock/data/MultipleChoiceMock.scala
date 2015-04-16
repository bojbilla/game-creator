package me.reminisce.mock.data

import me.reminisce.entities.Entities
import me.reminisce.entities.Entities.{MultipleChoiceQuestion, Possibility, Question}

/**
 * Created by roger on 09/11/14.
 */
class MultipleChoiceMock {
  private val user_id = "1234513"
  private val question1 = Question("Who is one of your friends?", Some(List("Pick the right one")), None)
  private val possiblities1 = Vector(Possibility(Some("100007638947117"), None),
    Possibility(Some("259477210905723"), None),
    Possibility(Some("100006296746302"), None),
    Possibility(Some("100004988686271"), None))

  private val question2 = Question("Who tagged you in this post?", Some(List("Great time in München")), None)
  private val possibilities2 = Vector(Possibility(Some("100007638947117"), None),
    Possibility(Some("259477210905723"), None),
    Possibility(Some("100006296746302"), None),
    Possibility(Some("100004988686271"), None))

  private val question3 = Question("Which page did you like?", Some(List("Pick the right one")), None)
  private val possiblities3 = Vector(
    Possibility(Some("Coca-Cola"), Some(
      """https://scontent-b-ams.xx.fbcdn.net/hphotos
        |-xpa1/v/t1.0-9/13571_10152140441473306_1421737798_n.jpg?oh=79061
        |403d9a86ca9a9e203479bcafb9b&oe=54E3852D""".stripMargin)),
    Possibility(Some("Google"), Some("https://fbcdn-sphotos-g-a.a" +
      "kamaihd.net/hphotos-ak-xaf1/v/t1.0-9/250036_10" +
      "151163421672838_2055278144_n.png?oh=05b6d63635648" +
      "201cf28865424e9a482&oe=54E02F29&__gda__=1423981986_f" +
      "357ba0dc9e11a764d420a8077e84374")),
    Possibility(Some("Apple Inc."), Some("https://fbcdn-sphotos-a-a.akamaihd.net" +
      "/hphotos-ak-xaf1/v/t1.0-9/552399_483957898301144_686906622_n" +
      ".jpg?oh=84c4ca3c00e0ae0dea342e6df4a541f5&oe=54E89B6E&__gda__=" +
      "1423333567_c902e5551f5350112dba8d2fc126a495")),
    Possibility(Some("Overwatch"), Some( """https://fbcdn-sphotos-g-a.akamaihd.net/""" +
      """hphotos-ak-xpa1/v/t1.0-9/1010150_300290316827287_1399766611443846150_n""" +
      """.png?oh=3a568fb906c2b5772ae3e5b4150e0f09&oe=54D2B90E&__gda__=1423122483""" +
      """_4c4a0539640baed6b8c2e53456086703"""))
  )

  val multiQuestion1 = MultipleChoiceQuestion(id = "12145", user_id,
    question1, possiblities1, 2)
  val multiQuestion2 = MultipleChoiceQuestion(id = "345232", user_id,
    question2, possibilities2, answer = 0)
  val multiQuestion3 = MultipleChoiceQuestion(id = "1231513", user_id,
    question3, possiblities3, answer = 3)


}
