package lectures.part1as

import scala.annotation.tailrec
import scala.jdk.Accumulator

object Recap extends App {
  val aCondition: Boolean = false
  val aConditionVal = if (aCondition) 42 else 65
  // Instructions (Procs) vs Expressions

  // Compiler can infer the type
  val aCodeBlock = {
    if (aCondition) 54
    56
  }

  // Unit -> It's like void...
  val theUnit = println("Hello, Scala")

  // Functions
  def aFunction(x: Int): Int = x + 1

  // Recursion: stack and tail
  @tailrec def factorial(n: Int, accumulator: Int): Int =
    if (n <= 0) accumulator
    else factorial(n - 1, n * accumulator)

  println(factorial(5, 1))

  // Object oriented programming

  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog // subtyping polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("Crunch!")
  }

  // method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog // natural language

  1.+(2) // a valid expression

  // Anonymous classes
  val classCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("Roar!")
  }

  classCarnivore.eat(aDog)

  // generic
  abstract class MyList[+A] // variance and variance problems in THIS course

  // singletons and compenions
  object MyList

  // case classes
  case class Person(name: String, age: Int)

  // exceptions and try/catch/finally
  // val throwsExceptions = throw new RuntimeException // Nothing
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "I caught an exception"
  } finally {
    println("some logs")
  }

  // packaging and imports

  // functional programming
  val incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1
  List(1, 2, 3).map(anonymousIncrementer)  // HOF
  // map, flatMap, filter

  // for-comprehension
  val pairs = for {
    num <- List(1, 2, 3)
    char <- List('a', 'b', 'c')
  } yield num + "-" + char

  println(pairs)

  // Scala collections: Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )
  // "collections": Options, Try
  val anOption = Some(2)

  // Pattern Matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }

  // all the patterns
}
