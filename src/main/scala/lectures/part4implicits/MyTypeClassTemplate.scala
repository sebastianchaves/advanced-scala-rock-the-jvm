package com.sebastianchaves.courses
package lectures.part4implicits

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
