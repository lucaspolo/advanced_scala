package lectures.part2afp

import scala.io.StdIn.readLine
import scala.util.control.Breaks.break

object PartialFunctions extends App{
  val aFunction = (x: Int) => x + 1  // Function[Int, Int] === Int => Int
  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  // {1, 2, 3} => Int

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // Partial Function value

  println(aPartialFunction(2))
//  print(aPartialFunction(41312))

  // Partial Function utilities
  println(aPartialFunction.isDefinedAt(67))

  // lift
  val lifted = aPartialFunction.lift  // Int => Option[Int]
  println(lifted(2))
  println(lifted(4123))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  println(pfChain(2))
  println(pfChain(45))

  // PF extends normal function

  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well
  val aMappedList = List(1,2,3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }

  println(aMappedList)

  /**
   * Note: PF can only have one parameter type
   */

  /**
   * Exercises
   *
   * 1 - Construct a partial function instance yourself (anonymous class)
   * 2 - Dumb chat bot
   */

  // 1

  val anAnonymousPartialFunction = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 1
      case 2 => 4
      case 3 => 9
    }

    override def isDefinedAt(x: Int): Boolean =
      if (List(1,2,3).contains(x)) true
      else false
  }

  // 2

  val aChatBot: PartialFunction[String, String] = {
    case "hi" => "Hi, how are you"
    case "what is your name?" => "My name is PFBot"
  }

  scala.io.Source.stdin.getLines().map(aChatBot).foreach(println)

}
