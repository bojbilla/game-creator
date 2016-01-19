package me.reminisce

/**
  * Provides actors and classes to interact with the mongo database.
  *
  * ==Overview==
  * The two main classes are:
  * - [[me.reminisce.database.MongoDatabaseService]], which allows the storage of posts, pages and fetch time in the
  * database
  *
  * - [[me.reminisce.database.DeletionService]], which allows the deletion of a user's data, the whole database or
  * duplicate likes
  */
package object database
