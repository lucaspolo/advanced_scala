package lectures.part1as

object AdvancedPatternMatching extends App {
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"The only element is $head")
    case _ =>
  }

  /**
   * - constants
   * - wildcards
   * - case classes
   * - tuples
   * - some special like above
   */

  class Person(val name: String, val age: Int)

  // defining an object
  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age <= 0) None
      else Some(person.name, person.age)

    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person(name = "Bob", age = 20)

  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a yo."
  }

  val legalStatus = bob.age match {
    case Person(status) => println(s"My legal status is $status")
  }

  /**
   * Exercise. 
   */

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10 && arg > -10
  }

  val n: Int = 8
  val mathProperty = n match {
    case singleDigit() => "single digit"
    case even() => "an even number"
    case _ => "no property"
  }

  println(mathProperty)
}
