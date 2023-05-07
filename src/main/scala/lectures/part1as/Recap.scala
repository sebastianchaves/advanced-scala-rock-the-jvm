package com.sebastianchaves.courses
package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {

  val aCondition: Boolean = false
  val aConditionedVal: Int = if (aCondition) 32 else 31

  // instructions (in sequence) vs expressions (declarations of logic)
  // compiler infers types for us

  // no sense in scala
  val aCodeBlock = {
    if (aCondition) 43
    45
  }

  // Unit = void => side effect
  val theUnit: Unit = println("Hi Scala")

  // functions
  def aFunction(x: Int): Int = x + 1

  // recursion: stack and tail
  // @tailrec => tail recursion

  @tailrec
  def factorial(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else factorial(n - 1, n * acc)

  // object-oriented programming
  class Animal
  class Dog extends Animal

  val aDog: Animal = new Dog // subtyping polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    def eat(a: Animal): Unit = println("yummy")
  }

  // method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog // infix notation

  // anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("rico")
  }

  // generics
  // + => means covariance
  abstract class MyList[+A] // variance and variance problems

  // singleton and companions
  object MyList

  // case classes
  case class Person(name: String, age: Int)

  // exceptions and try/catch/finally

  val throwsException: Nothing = throw new RuntimeException

  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "Some error occurred"
  } finally {
    println("Closing all")
  }

  // packages and imports

  // functional programming
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer: Int => Int = (x: Int) => x + 1
  List(1,2,3).map(anonymousIncrementer) // high order functions = HOFs
  // map, flatMap, filter

  // for-comprehensions
  val pairs = for {
    num   <- List(1, 2, 3)
    char  <- List('a', 'b', 'c')
  } yield num + "-" + char

  // Scala collections: Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val aMap = Map("Daniel" -> 133, "Ricardo" -> 123, "Roberta" -> 654)

  // "collections": Options, Trys => Monads
  val anOption = Option(2)

  // pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 32)
  val greeting = bob match {
    case Person(name, _) => s"Hi, my name is $name"
  }

  // all the patterns

}
