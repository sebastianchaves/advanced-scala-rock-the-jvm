package com.sebastianchaves.courses
package lectures.part4implicits

object TypeClasses extends App {

  // Part 1
  // option 1
  trait HTMLWritable {
    def toHTML: String
  }

  case class User(name: String, age: Int, email: String)

  User("Carlos", 55, "carlos@yahoo.com")

  /* disadvantages
    1 - only works for the types WE write
    2 - only ONE implementation out of quite a number
  */

  // option 2 - pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any) = value match {
      case User(_, _, _) => ???
      case _ => ???
    }
  }

  /* disadvantages
  1 - lost type safety
  2 - need to modify the code every time
  3 - still ONE implementation
*/

  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  implicit object UserSerializer extends HTMLSerializer[User] {
    override def serialize(user: User): String = s"<div>${user.name} (${user.age} yo) <a href=${user.email}/></div>"
  }

  val carlos = User("Carlos", 55, "carlos@yahoo.com")
  println(UserSerializer.serialize(carlos))

  // Benefits
  // 1 - we can define serializers for other types
  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(date: Date): String = s"<div>${date.toString}</div>"
  }

  // 2 - we can define multiple serializers
  object PartialUserSerializer extends HTMLSerializer[User] {
    override def serialize(user: User): String = s"<div>${user.name}</div>"
  }

  // 3 - not lost type safety

  // Type class
  trait MyTypeClassTemplate[T] {
    def action(value: T): Any
  }
  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]): MyTypeClassTemplate[T] = instance
  }

  // Type class instances
  object MyTypeClassInstance extends MyTypeClassTemplate[String] {
    override def action(value: String): Any = ???
  }

  /* Exercise - Equality */
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object EmailEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.email == b.email
  }

  NameEquality(User("carlos", 22, "carlos@gmail.com"), User("roberto", 45, "roberto@gmail.com"))

  // Part 2
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)

    def apply[T](serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div>$value</div>"
  }

  println(HTMLSerializer.serialize(22))

  /* Exercise - Equality Type Class */
  // AD-HOC polymorphism
  trait Equality[T] {
    def compare(a: T, b: T): Boolean
  }
  object Equality {
    def compare[T](a: T, b: T)(implicit equal: Equality[T]): Boolean = equal.compare(a, b)
    def apply[T](equal: Equality[T]): Equality[T] = equal
  }

  implicit object IntEquality extends Equality[Int] {
    override def compare(a: Int, b: Int): Boolean = a == b
  }

  println(Equality.compare(1, 2))

  // Part 3
  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  println(carlos.toHTML)
  println(2.toHTML)

  /* Benefits:
    1. extend to new types
    2. choose implementation
    3. super expressive!
  */

  /* Main elements of enhance type with type classes:
    1. type class itself
    HTMLSerializer[T] { }

    2. type class instances (some of which are implicit)
    UserSerializer, IntSerializer, etc

    3. conversion with implicit classes
    HTMLEnrichment[T] { }
  */

  // context bounds
  def htmlBoilerplate[T](content: T)(implicit serializer: HTMLSerializer[T]): String =
    s"<html><body>${content.toHTML(serializer)}</body></html>"

  def htmlSugar[T : HTMLSerializer](content: T): String = {
    val serializer = implicitly[HTMLSerializer[T]]
    // use serializer
    s"<html><body>${content.toHTML(serializer)}</body></html>"
  }

  // implicitly
  case class Permissions(mask: String)
  implicit val defaultPermissions: Permissions = Permissions("0744")

  // in some other part of the code
  val standardPerms = implicitly[Permissions]

  // Recap
  // Type class is a trait which take a type parameter and define some operations/actions
  trait MyTypeClass[T] {
    def action(value: T): Any
  }
  // Type class instances which implements these operations for specific types, those instances are often implicits
  implicit object MyTCInstance extends MyTypeClass[Int] {
    override def action(value: Int): Any = ???
  }
  // Invocation
  // 1. with companion object that known implicitly the specific type class instance
  object MyTypeClass {
    def apply[T](implicit instance: MyTypeClass[T]): MyTypeClass[T] = instance
  }

  // 2. enrich type with implicit class
  implicit class ConversionClass[T](value: T) {
    def action(implicit instance: MyTypeClass[T]): Any = instance.action(value)
  }

}
