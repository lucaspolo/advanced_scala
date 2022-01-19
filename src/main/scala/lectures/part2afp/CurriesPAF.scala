package lectures.part2afp

object CurriesPAF extends App {
  // curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3)  // Int => Int = y => 3 + y
  println(add3(5))
  println(superAdder(3)(5))

  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int = x + y  // curried method

  val add4: Int => Int = curriedAdder(4)  // lifting = ETA-EXPANSION

  // function != methods (JVM limitation)
  def inc(x: Int) = x + 1
  List(1,2,3).map(inc)  // ETA-expansion: compiler expands 'inc'

  // Partial function applications
  val add5 = curriedAdder(5) _  // The '_' force compiler to do ETA-expansion: Int => Int

  /**
   * Exercise
   */
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7 = (x: Int) => simpleAddFunction(7, x)  // simplest
  val add7_2 = simpleAddFunction.curried(7)
  val add7_6 = simpleAddFunction(7, _: Int)

  val add7_3 = curriedAddMethod(7) _  // ETA-expansion
  val add7_4 = curriedAddMethod(7)(_)  // PAF = alternative syntax

  val add7_5 = simpleAddMethod(7, _: Int)  // alternative syntax for turning methods into function values
  // y => simpleAddMethod(7, y)

  // underscore are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")
  println(insertName("Lucas"))

  val fillInTheBlanks = concatenator("Hello ", _: String, _: String)
  println(fillInTheBlanks("Lucas", " Polo"))

  /**
   * Exercises
   * 1. Process a list of numbers and return their string representations with different formats
   * Use the %4.2f, %8.6g and %14.12f with a curried a formatter function
   */
  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  val simpleFormat = curriedFormatter("%4.2f") _  // lift
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _

  println(numbers.map(curriedFormatter("%16.16f")))  // compiler infers the eta-expansion

  println(numbers.map(simpleFormat))
  println(numbers.map(seriousFormat))
  println(numbers.map(preciseFormat))

  /**
   * 2. Difference between
   *  - functions vs methods
   *  - parameters: by-name vs 0-lambda
   *
   */

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  byName(23)  // ok
  byName(method)  // ok
  byName(parenMethod())
//  byName(parenMethod)  // ok but beware (in Scala 2) (not OK in Scala 3)
//  byName(() => 42)  // not ok
  byName((() => 42)())  // ok
//  byName(parenMethod _)

//  byFunction(45)
  byFunction(() => 45)
//  byFunction(method)
//  byFunction(method)  compiler does not do ETA-expansion
  byFunction(parenMethod)
  byFunction(parenMethod _)  // also works (unnecessary)

}
