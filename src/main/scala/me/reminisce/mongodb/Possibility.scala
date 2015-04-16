package me.reminisce.mongodb

import reactivemongo.bson._


case class Possibility(id: Option[BSONObjectID] = None, text: String, imageUrl: String)


object Possibility {

  implicit val posibilityf = Macros.handler[Possibility]
}

