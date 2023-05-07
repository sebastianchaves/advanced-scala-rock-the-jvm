package com.sebastianchaves.courses
package lectures.part1as

import scala.util.Try

object DarkSugars extends App {

  // syntax sugar #1: methods with single param
  def singleArgMethod(arg: Int): String = s"your arg is: $arg"

  val description = singleArgMethod {
    // some complex code
    32
  }

  val aTry = Try {
    throw new RuntimeException
  }

  List(1,2,3).map { x =>
    x + 1
  }

  // syntax sugar #2: single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val anInstanceWithSugar: Action = (x: Int) => x + 1 // compiler magic

  // example: Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Soy un runnable")
  })

  val aSweeterThread = new Thread(() => println("Soy un runnable cheto"))

  abstract class AnAbstractType {
    def implemented: Int = 2
    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("asd")

  // syntax sugar #3: the :: and #:: methods are special
  val prependedList = 2 :: List(3, 4)
  // like 2.::(List(3,4)) => but Int dont have :: method
  // compiler translate to => List(3,4).::(2) how?

  // scala specification: last character decides associativity of method
  val numbers = 1 :: 2 :: 3 :: List(4, 5)
  val theSameNumbers = List(3, 4).::(3).::(2).::(1)

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4: multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "Scala 4ever"

  // syntax sugar #5: infix types
  class Composite[A, B]
  val composite: Composite[Int, String] = ???
  val infixTypeComposite: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???

  // syntax sugar #6: update() is very special, much like apply()
  val anArray = Array(1,2,3)
  anArray(2) = 7 // rewritten to anArray.update(2, 7)
  // used in mutable collections
  // remember apply() and update() !

  // syntax sugar #7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member: Int = internalMember // getter
    def member_=(value: Int): Unit = internalMember = value // setter
  }

  val aMutableContainer: Mutable = new Mutable
  aMutableContainer.member = 42 // rewritten as aMutableContainer member =(42)

}
