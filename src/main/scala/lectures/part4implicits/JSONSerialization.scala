package com.sebastianchaves.courses
package lectures.part4implicits

import java.util.Date

object JSONSerialization extends App {

  // Users, posts, feeds
  // serialize to json

  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date, isActive: Boolean)
  case class Feed(user: User, post: List[Post])

  /*
    1- intermediate data types: Int, String, List, Date
    2- type classes for conversion, from case classes to intermediate data types
    3- serialize to JSON
  */

  // intermediate data type
  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    override def stringify: String = values.map {
      case (key, value) => s"\"$key\":${value.stringify}"
    }.mkString("{", ",", "}")
  }

  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    override def stringify: String = values.map(_.stringify).mkString("[", ",", "]")
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = s"\"$value\""
  }

  final case class JSONBoolean(value: Boolean) extends JSONValue {
    override def stringify: String = if (value) "true" else "false"
  }

  val data = JSONObject(Map(
    "user" -> JSONString("Daniel"),
    "posts" -> JSONArray(List(
      JSONString("Dale pa"),
      JSONNumber(123)
    ))
  ))
  println(data.stringify)

  // type class
  // 1. type class
  // 2. type class instances (implicit)
  // 3. pimp library to use type class instances

  // 1. type class
  trait JSONConverter[T] {
    def convert(value: T): JSONValue
  }

  // 2.1 conversion
  implicit class JSONOps[T](value: T) {
    def toJSON(implicit converter: JSONConverter[T]): JSONValue = converter.convert(value)
  }

  // 2. type class instances (implicit)
  implicit object StringConverter extends JSONConverter[String] {
    override def convert(value: String): JSONValue = JSONString(value)
  }

  implicit object NumberConverter extends JSONConverter[Int] {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }

  implicit object UserConverter extends JSONConverter[User] {
    override def convert(user: User): JSONValue = JSONObject(Map(
     "name" -> JSONString(user.name),
     "age" -> JSONNumber(user.age),
     "email" -> JSONString(user.email)
    ))
  }

  implicit object PostConverter extends JSONConverter[Post] {
    override def convert(post: Post): JSONValue = JSONObject(Map(
      "content" -> JSONString(post.content),
      "created_at" -> JSONString(post.createdAt.toString),
      "is_active" -> JSONBoolean(post.isActive)
    ))
  }

  implicit object FeedConverter extends JSONConverter[Feed] {
    override def convert(feed: Feed): JSONValue = JSONObject(Map(
      "user" -> feed.user.toJSON,
      "posts" -> JSONArray(feed.post.map(_.toJSON))
    ))
  }

  // use
  val now = new Date(System.currentTimeMillis())
  val john = User("John", 34, "john@yahoo.com.ar")
  val feed = Feed(john, List(
    Post("hello", now, false),
    Post("bye", now, true)
  ))

  case class Carlos(name: String)

  val carlos = Carlos("charly")

  println(feed.toJSON.stringify)
  println(feed.toJSON)

}
