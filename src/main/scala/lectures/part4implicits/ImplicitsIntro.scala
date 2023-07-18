package com.sebastianchaves.courses
package lectures.part4implicits

object ImplicitsIntro extends App {

  val pair = "Cheb" -> "123"

  case class Person(name: String) {
    def greet = s"Holas soy $name"
  }

  implicit def fromStringToPerson(str: String): Person = Person(str)

  println("Peter".greet) // println(fromStringToPerson("Peter").greet)

  // ambiguous example
//  class A {
//    def greet = "asd"
//  }
//  implicit def fromStringToA(str: String): A = new A

  // implicit parameters
  // different to default parameters
  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val defaultAmount: Int = 30
  increment(2)

  


}
