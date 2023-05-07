package com.sebastianchaves.courses
package lectures.part1as

object AdvancedPatternMatching extends App {

  val numbers = List(1)
  val description: Unit = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ => ()
  }

  /*
    - constants
    - wildcards
    - case class
    - tuples
    - some special magic like above
  */

  class Person(val name: String, val age: Int)

  // PersonPattern or Person no matter the name
  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name, person.age))

    def unapply(age: Int): Option[String] =
      Some(if (age <21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case Person(name, age) => s"Hi my name is $name and i am $age years old"
  }

  println(greeting)

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(legalStatus)

  /*
  * Exercise
  * */
  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10 && arg > -10
  }

  val n: Int = 44
  val mathProperty = n match {
    case singleDigit() => "single digit"
    case even() => "is even"
    case _ => "no property"
  }

  println(mathProperty)

  // infix patterns => only with two params
  case class Or[A, B](a: A, b: B) // Either

  val either = Or(2, "two")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }
  println(humanDescription)

  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty) else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "Starting with 1, 2"
    case _ => "something else"
  }

  println(decomposed)

  // custom return types for unapply
  // isEmpty: Boolean, get: something
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  // the unapply method only need to return a type that implements 2 methods: isEmpty and get (like Option)
  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty: Boolean = false
      def get: String = person.name
    }
  }

  println(bob match {
    case PersonWrapper(n) => s"this person name is $n"
    case _ => "An alien"
  })

}
