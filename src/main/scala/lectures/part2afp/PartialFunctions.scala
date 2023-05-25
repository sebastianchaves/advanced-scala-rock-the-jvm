package com.sebastianchaves.courses
package lectures.part2afp

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int

  val aFussyFunction = (x: Int) =>
    if (x == 1) 43
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 43
    case 2 => 56
    case 5 => 999
  }

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 43
    case 2 => 56
    case 5 => 999
  } // partial function value

  println(aPartialFunction(2))

  // Partial Functions Utils
  aPartialFunction.isDefinedAt(33) // false

  // lift
  val lifted = aPartialFunction.lift // Int => Option[Int]
  lifted(2) // Some(56)
  lifted(33) // None

  val chained = aPartialFunction.orElse[Int, Int] {
    case 54 => 44
  }

  chained(2) // 56
  chained(54) // 44

  // Partial Functions extends normal functions
  // Partial Functions are a subtype of total functions
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accepts partial functions aswell
  val aMappedList = List(1,2,3).map {
    case 1 => 99
    case 2 => 53
    case 3 => 1000
  }

  /*
    Note: Partial Functions can only have ONE parameter type
  */

  // Exercises
  // 1. construct a PF instance yourself (anonymous class)
  // 2. dumb chat bot as a PF

  val aManualFussyFunction = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 43
      case 2 => 56
      case 5 => 999
    }

    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 5
  }

  val chatbot: PartialFunction[String, String] = {
    case "hello" => "Hi, im a chatbot"
    case "goodbye" => "BYE"
    case "call roberto" => "Calling ROBERTO..."
  }

  scala.io.Source.stdin.getLines.map(chatbot).foreach(println)

  val set = Set(1,2,3)


}
