package com.sebastianchaves.courses
package lectures.part4implicits

object PimpMyLibrary extends App {

  // type enrichment = pimping

  // 2.isPrime
  implicit class RichInt(val value: Int) extends AnyVal { // extends AnyVal for memory optimization
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)

    def times(f: () => Unit): Unit = {
      def aux(n: Int): Unit =
        if (n <= 0) ()
        else {
          f()
          aux(n -1)
        }
        aux(value)
    }

    def *[T](list: List[T]): List[T] = {
      def concatenate(n: Int): List[T] =
        if (n <= 0) List()
        else concatenate(n - 1) ++ list

      concatenate(value)
    }
  }

  2.isEven // new RichInt(2).isEven

  1 to 10

  import scala.concurrent.duration._
  3.seconds

  // compiler doesn't do multiple implicit searches
  implicit class RicherInt(richInt: RichInt) {
    def isOdd: Boolean = richInt.value % 2 != 0
  }

  // 2.isOdd // not compile

  /* Exercise - RichString
  1. asInt
  2. encrypt
  3. times(f: function)
  4. *
  */
  implicit class RichString(s: String) {
    def asInt: Int = Integer.valueOf(s) // java.lang.Integer => Int
    def encrypt(cypherDistance: Int): String = s.map(c => (c + cypherDistance).asInstanceOf[Char])
  }

  println("3".asInt + 7)
  println("John".encrypt(4))

  3.times(() => println("asd"))
  3 * List(1,2,3)

  // implicit conversions
  implicit def stringToInt(string: String): Int = Integer.valueOf(string)
  println("2" / 2) // stringToInt("6") / 2

  // equivalent to implicit class
  class RichAltInt(value: Int)
  implicit def enrich(value: Int): RichAltInt = new RichAltInt((value))

  // danger zone

  // Tips
  /*
    - keep type enrichment to implicit classes and type classes
    - avoid implicit defs as much as possible
    - package implicit clearly, bring into scope only what you need
    - IF you need conversions, make them specific
    - NEVER with primitive types
  */

}
