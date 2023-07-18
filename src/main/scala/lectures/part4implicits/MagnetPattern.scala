package com.sebastianchaves.courses
package lectures.part4implicits

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

object MagnetPattern extends App {

  // method overloading

  class P2PRequest
  class P2PResponse
  class Serializer[T]

  trait Actor {
    def receive(statusCode: Int): Int
    def receive(request: P2PRequest): Int
    def receive(response: P2PResponse): Int
    def receive[T : Serializer](message: T): Int
    def receive[T : Serializer](message: T, statusCode: Int): Int
    def receive(future: Future[P2PRequest]): Int
    // def receive(future: Future[P2PResponse]): Int // Example of type erasure in overloading
  }

  /* Problems:
    1- type erasure
    2- lifting doesn't work for all overloads

      val receiveFV = receive _ // which type?

    3- code duplication
    4- type inference and default args

    actor.receive(?!)
  */

  trait MessageMagnet[Result] {
    def apply(): Result
  }

  def receive[R](magnet: MessageMagnet[R]): R = magnet()

  implicit class FromP2PRequest(request: P2PRequest) extends MessageMagnet[Int] {
    def apply(): Int = {
      println("Handling P2PRequest")
      24
    }
  }

  implicit class FromP2PResponse(response: P2PResponse) extends MessageMagnet[Int] {
    def apply(): Int = {
      println("Handling P2PResponse")
      54
    }
  }

  receive(new P2PRequest)
  receive(new P2PResponse)

  // Benefits
  // 1. no more type erasure problems
  implicit class FromResponseFuture(future: Future[P2PResponse]) extends MessageMagnet[Int] {
    override def apply(): Int = 2
  }
  implicit class FromRequestFuture(future: Future[P2PRequest]) extends MessageMagnet[Int] {
    override def apply(): Int = 4
  }

  println(receive(Future(new P2PResponse)))
  println(receive(Future(new P2PRequest)))

  // 2. lifting works only when magnet return always the same type
  trait MathLib {
    def add1(x: Int): Int = x + 1
    def add1(s: String): Int = s.toInt + 1
    // another overloads
  }

  // "magnetize"
  trait AddMagnet {
    def apply(): Int
  }

  def add1(magnet: AddMagnet): Int = magnet()

  implicit class AddInt(x: Int) extends AddMagnet {
    override def apply(): Int = x + 1
  }

  implicit class AddString(s: String) extends AddMagnet {
    override def apply(): Int = s.toInt + 1
  }

  val addFunctionValue = add1 _

  println(addFunctionValue(1))
  println(addFunctionValue("2"))

//  val receiveFunctionValue = receive _
//  receiveFunctionValue(new P2PRequest)

  /* Drawbacks
    1. verbose
    2. harder to read
    3. you cant name or place default arguments
    4. call by name doesnt work correctly
   */

  // drawback 4 example:
  class Handler {
    def handle(s: => String): Unit = {
      println(s)
      println(s)
    }
    // other overloads
  }

  trait HandlerMagnet {
    def apply(): Unit
  }

  def handle(magnet: HandlerMagnet): Unit = magnet()

  implicit class StringHandle(s: => String) extends HandlerMagnet {
    override def apply(): Unit = {
      println(s)
      println(s)
    }
  }

  def sideEffectMethod(): String = {
    println("Hello, Scala")
    "hahaha"
  }

  handle(sideEffectMethod())
  handle {
    println("Hello, Scala")
    "hahaha"
  }

}
