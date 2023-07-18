package com.sebastianchaves.courses
package exercises

import lectures.part4implicits.TypeClasses.User

object EqualityPlayground {

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object EmailEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.email == b.email
  }
  
  val carlos = User("carlos", 22, "carlos@gmail.com")
  
  // Exercise - improve Equal TC with implicit conversion class
  // ===(another value: T)
  // !==(another value: T)

  implicit object IntEqual extends Equal[Int] {
    override def apply(a: Int, b: Int): Boolean = a == b
  }

  implicit class TypeSafeEqual[T](value: T) {
    def ===(another: T)(implicit equalizer: Equal[T]): Boolean = equalizer(value, another)
    def !==(another: T)(implicit equalizer: Equal[T]): Boolean = !equalizer(value, another)
  }

  val roberto: User = User("Roberto", 12, "mailderoberto@asd.com")

  carlos === roberto
  /*
    carlos.===(roberto)
    new TypeSafeEqual[User](carlos).===(roberto)
    new TypeSafeEqual[User](carlos).===(roberto)(NameEquality)
  */
  // Advantages
  // TYPE SAFE
  // carlos === 32

  
  
}
