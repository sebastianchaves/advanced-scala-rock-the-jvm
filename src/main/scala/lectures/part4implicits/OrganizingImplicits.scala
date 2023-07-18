package com.sebastianchaves.courses
package lectures.part4implicits

object OrganizingImplicits extends App {

  implicit def reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
//  implicit val normalOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)

  println(List(1, 5, 2, 4, 3).sorted)

  // scala.Predef

  /*
    Implicits (used as implicit parameters):
      - val/var
      - objects
      - accessor methods = defs with no parentheses
  */

  // Exercises
  case class Person(name: String, age: Int)

//  object Person {
//    implicit def alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.name.compareTo(p2.name) < 0)
//  }
//  implicit def ageOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.age < p2.age)

  object AlphabeticNameOrdering {
    implicit def alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.name.compareTo(p2.name) < 0)
  }

  object AgeOrdering {
    implicit def ageOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.age < p2.age)
  }

  import AlphabeticNameOrdering.alphabeticOrdering

  val persons = List(Person("Carlos", 46), Person("Roberto", 76), Person("Ana", 33))
  println(persons.sorted)

  /*
    Implicit scope priority
    1. normal scope = LOCAL SCOPE
    2. imported scope
    3. companions of all types involved in the method signature
      Example:
        `persons.sorted`
      - List
      - Ordering
      - all the types involved = A or any supertype
  */

  /* Best Practices
    When defining an implicit val
    #1
    - if there is a single possible value for it
    - and you can edit the code for the type
    then define the implicit in the companion

    #2
    - if there is are many possible values for it
    - but a single good one
    - and you can edit the code for the type
    then define the good implicit in the companion
  */

  /*
    Exercise
    - totalPrice = most used (50%)
    - byUnitCount = 25%
    - byUnitPrice = 25%
  */
  case class Purchase(nUnits: Int, unitPrice: Double) {
    def totalPrice: Double = nUnits * unitPrice
  }
  object Purchase {
    implicit val totalPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((p1, p2) => p1.totalPrice < p2.totalPrice)
  }

  object UnitCountOrdering {
    implicit val unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits < _.nUnits)
  }

  object UnitPriceOrdering {
    implicit val unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }

}
