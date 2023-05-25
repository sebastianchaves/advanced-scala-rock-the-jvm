package com.sebastianchaves.courses
package lectures.part2afp

object LazyEvaluation extends App {

  // lazy DELAYS the evaluation of values
  lazy val x: Int = throw new RuntimeException
  // println(x)

  lazy val y: Int = {
    println("hello")
    42
  }
  // only print one time
  println(y)
  println(y)

  // examples of implications:
  // side effects
  def sideEffectCondition: Boolean = {
    println("asd")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = {
    // CALL BY NEED
    lazy val t = n // only evaluated once
    t + t + t + 1
  }
  def retrieveMagicValue = {
    // side effect or a long computation
    println("waiting")
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val less30 = numbers.filter(lessThan30) // List(1, 25, 5, 23)
  val greater20 = less30.filter(greaterThan20)
  println(greater20)

  val lt30Lazy = numbers.withFilter(lessThan30) // lazy values under the hood
  val gt20Lazy = lt30Lazy.withFilter(greaterThan20) // lazy values under the hood

  gt20Lazy.foreach(println)

  // for-comprehensions use withFilter with guards
  for {
    a <- List(1,2,3) if a % 2 == 0 // use lazy values
  } yield a + 1
  // equals to
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1)

}
