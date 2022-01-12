package lectures.part1as

import scala.util.Try

object DarkSugars extends App {
  // syntax sugar #1: methods with single param
  def singleArgumentMethod(arg: Int): String = s"$arg little ducks..."

  val description = singleArgumentMethod {
    // write some code
    42
  }

  println(description)

  val aTryInstance = Try {  // Java's try {...}
    throw new RuntimeException
  }

  List(1,2,3).map { x =>
    x + 1
  }

  // syntax sugar #2: single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val anotherInstance: Action = (x: Int) => x + 1  // compiler converts into interface, like Java

  // example: Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello")
  })

  val aSweeterThread = new Thread(() => println("Hello Scala"))

  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // syntax sugar #3: the :: and #:: methods are special
  val prependList = 2 :: List(2,3)
  // 2.::(List(3,4)) wrong
  // List(3,4).::2

  // scala spec: last char decides associativity of method (in example, it's posfixed)

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4: multi-word method naming

  class TeenGirl(name: String) {
    def `and then said`(gossip: String) = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "Scala is so sweet!"

  // syntax sugar #5: infix types
  class Composite[A, B]
  val composite: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???

  // syntax sugar #6: update() is very special, much like apply()
  val anArray = Array(1,2,3)
  anArray(2) = 7 // rewritten to anArray.update(2, 7)
  // used in mutable collections
  // remember apply() AND update()

  // syntax sugar #7: setter for muttable containers
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member = internalMember // "getter"
    def member_=(value: Int): Unit = internalMember = value // "setter"
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42
}
