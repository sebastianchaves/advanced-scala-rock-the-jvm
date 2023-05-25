package com.sebastianchaves.courses
package lectures.part2afp

object CurriesPAF extends App {

  val supperAdder: Int => Int => Int = x => y => x + y

  val add3 = supperAdder(3) // Int => Int = y => y + 3

  println(add3(5))
  println(supperAdder(3)(5)) // curried function

  // METHOD
  def curriedAdder(x: Int)(y: Int): Int = x + y

  // lifting or ETA-EXPANSION => transform a method to function
  val add4: Int => Int = curriedAdder(4)

  // functions != methods (JVM limitation)

  def inc(x: Int) = x + 1
  List(1, 2, 4).map(inc) // ETA-expansion => turns inc into a function

  // Partial function applications
  val add5 = curriedAdder(5) _ // Int => Int

  // Exercise
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int
  val add7_1 = (x: Int) => simpleAddFunction(7, x)
  val add7_2 = simpleAddFunction.curried(7)
  val add7_6 = simpleAddFunction(7, _: Int)

  val add7_3 = curriedAddMethod(7) _ // partially applied function
  val add7_4 = curriedAddMethod(7)(_) // partially applied function

  val add7_5 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello, im ", _: String, ", how are you?") // x: String => concatenator(asd, x, asd)
  println(insertName("Ricardo"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // (x, y) => concatenator("Hello ", x, y)
  println(fillInTheBlanks("JUJU", " XD"))

  // Exercise
  // 1. Process a list of numbers and return their string representations with different formats
  // Use %4.2f %8.6f %14.12f with a curried formatter function

  def curriedFormatter(f: String)(n: Double): String = f.format(n)

  val format42 = curriedFormatter("%4.2f")
  val format86 = curriedFormatter("%8.6f")
  val format1412 = curriedFormatter("%14.12f")

  List(1.22, Math.PI, Math.E).map(format42)

  // 2. Difference between
  // functions vs methods
  // parameters: by-name vs 0-lambda
  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 32
  def parenMethod(): Int = 12

  /*
    calling byName and byFunction
    - int
    - method
    - parenMethod
    - lambda
    - partially applied function
  */
  byName(42)
  byName(method)
  byName(parenMethod())
  // byName(parenMethod) not ok
  // byName(() => 43) not ok
  byName((() => 43)())
  // byName(parenMethod _) not ok

  // byFunction(45) not ok
  // byFunction(method) not ok
  byFunction(parenMethod) // compiler does ETA-expansion
  byFunction(() => 23)
  byFunction(parenMethod _)

}
