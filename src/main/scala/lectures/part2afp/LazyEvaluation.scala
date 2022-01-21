package lectures.part2afp

object LazyEvaluation extends App {
  // Evaluated only when used
  lazy val x: Int = {
    println("hello")
    42
  }
  println(x)
  println(x)  // In second use not evaluate block again

  // examples and implications
  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val LazyCondition = sideEffectCondition
  println(if (simpleCondition && LazyCondition) "yes" else "no")  // You don't see Boo because && makes short-circuit

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = {
    lazy val t = n  // This will "unpack" n to not wait three computations // CALL BY NEED
    t + t + t + 1
  }
  def retrieveMagicValue = {
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))
  // use lazy vals

  // Filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = numbers.filter(greaterThan20)

  println(gt20)

  val lt30Lazy = numbers.withFilter(lessThan30)  // Lazy values under the hood
  val gt20Lazy = numbers.withFilter(greaterThan20)
  println
  gt20Lazy.foreach(println)

  // for-comprehensions use withFilter with guards
  for {
    a <- List(1, 2, 3) if a % 2 == 0
  } yield a + 1
  // is the same that
  List(1,2,3).withFilter(_ % 2 == 0 ).map(_ + 1)

  /**
   * Exercise implement a lazily evaluated, singly linked STREAM of elements
   */

  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B]  // prepend operador
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]  // concatenate two streams

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A]  // takes the first n elements out of this stream
    def takeAsList(n: Int): List[A]
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = ???
  }
}
