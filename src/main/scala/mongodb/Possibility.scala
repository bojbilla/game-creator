package mongodb

import reactivemongo.bson.Macros.Options.{\/, UnionType, AllImplementations}
import reactivemongo.bson._


case class Possibility(id: Option[BSONObjectID] = None, text: String, imageUrl: String)


object Possibility {

  implicit val posibilityf = Macros.handler[Possibility]
}

