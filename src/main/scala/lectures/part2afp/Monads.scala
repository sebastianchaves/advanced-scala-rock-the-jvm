package com.sebastianchaves.courses
package lectures.part2afp

object MonadsTheory extends App {

  // Monads are a kind of types which have some fundamental ops:
  // 1. unit also called pure or apply
  // 2. flatMap also called bind
  trait MonadTemplate[A] {
    def unit(value: A): MonadTemplate[A]
    def flatMap[B](f: A => MonadTemplate[B]): MonadTemplate[B]
  }

  // List, Option, Try, Future, Stream, Set are all monads

  // Operations must satisfy the monad laws:
  // 1- left-identity => unit(x).flatMap(f) == f(x)
  // Example: List
  val f: Int => List[Int] = (x: Int) => List(x + 1)

  List(1).flatMap(f) // equals to
  f(1) ++ Nil.flatMap(f) // equals to
  f(1)

  // 2- right-identity => aMonad.flatMap(unit) == aMonad
  List(1).flatMap(List(_)) // equals to
  List(1)

  // 3- associativity => aMonad.flatMap(f).flatMap(g) == aMonad.flatMap(x => f(x).flatMap(g))
  val g: Int => List[Int] = (x: Int) => List(x + 2)

  List(1, 2, 3).flatMap(f).flatMap(g) // equals to
  (f(1) ++ f(2) ++ f(3)).flatMap(g) // equals to
  f(1).flatMap(g) ++ f(2).flatMap(g) ++ f(3).flatMap(g) // equals to

  List(1, 2, 3).flatMap(f(_).flatMap(g)) // equals to
  List(1, 2, 3).flatMap(x => f(x).flatMap(g))

  // Example: Options
  // 1- left-identity
  val h: Int => Option[Int] = (x: Int) => Option(x + 1)
  val j: Int => Option[Int] = (x: Int) => Option(x + 2)

  Option(1).flatMap(h) // equals to
  h(1)

  Some(1).flatMap(h)
  None.flatMap(h)

  // 2- right-identity
  Some(1).flatMap(Option(_)) // equals to
  Option(1) // equals to
  Some(1)

  // 3. associativity
  val o = Option(1)
  // A
  o.flatMap(h).flatMap(j)

  // B
  o.flatMap(x => h(x).flatMap(j))

  // A must be equal to B
  // A
  o.flatMap(h).flatMap(j) // equals to
  h(1).flatMap(j)

  o.flatMap(x => h(x).flatMap(j)) // equals to
  h(1).flatMap(j)

}

object Monads extends App {

  // our own Try Monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }
  object Attempt {
    def apply[A](value: => A): Attempt[A] =
      try {
        Success(value)
      } catch {
        case e: Throwable => Failure(e)
      }
  }

  case class Success[A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Failure(e)
      }
  }

  case class Failure(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  // left-identity
  // unit.flatMap(f) == f(x)
  // Attempt(x).flatMap(f) == f(x) // success case
  // Success(x).flatMap(f) // proved

  // right-identity
  // attempt.flatMap(unit) == attempt
  // Success(x).flatMap(x => Attempt(x)) == Attempt(x) == Success(x)
  // Failure(x).flatMap(x => Attempt(x)) == Attempt(x) == Failure(x)

  // associativity
  // attempt.flatMap(f).flatMap(g) == attempt.flatMap(x => f(x).flatMap(g))
  // Failure(e).flatMap(f).flatMap(g) == Failure(e)
  // Failure(e).flatMap(x => f(x).flatMap(g)) == Failure(e)

  // Success(x).flatMap(f).flatMap(g) == f(x).flatMap(g) || Failure(e)
  // Success(x).flatMap(x => f(x).flatMap(g)) == f(x).flatMap(g) || Failure(e)

  val attempt = Attempt {
    throw new RuntimeException("Error")
  }

  println(attempt)

  /*
  * Exercise:
  * 1) implement a Lazy[T] monad = computation which will only be executed when it's needed
  * unit/apply
  * flatMap

  * 2) Monads = unit + flatMap
    Monads = unit + map + flatten

    Monad[T] {
      def flatMap[B](f: T => Monad[B]): Monad[B] = ... (implemented)

      def map[B](f: T => B): Monad[B] = ???
      def flatten(m: Monad[Monad[T]]): Monad[T] = ???

      (have list in mind)
    }
  * */

  // 1
  class Lazy[+A](value: => A) {
    private lazy val delayed: A = value // call by need
    def use: A = value
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(delayed)

    // 2 exercise
    def map[B](f: (=> A) => B): Lazy[B] = flatMap(x => Lazy(f(x)))
    def flatten[B](m: Lazy[Lazy[B]]): Lazy[B] = m.flatMap(x => x)
  }
  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("JEJE im lazy boy")
    42
  }

  val flatMappedInstance1 = lazyInstance.flatMap(x => Lazy(10 * x))
  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy(10 * x))

  flatMappedInstance1.use
  flatMappedInstance2.use

  /*
  left-identity
  unit.flatMap(f) == f(v)
  Lazy(v).flatMap(f) == f(v)

  right-identity
  l.flatMap(unit) == l
  Lazy(v).flatMap(x => Lazy(x)) == Lazy(v)

  associativity
  l.flatMap(f).flatMap(g) == l.flatMap(x => f(x).flatMap(g))

  Lazy(v).flatMap(f).flatMap(g) == f(v).flatMap(g)

  Lazy(v).flatMap(x => f(x).flatMap(g)) == f(v).flatMap(g)
  */

}
