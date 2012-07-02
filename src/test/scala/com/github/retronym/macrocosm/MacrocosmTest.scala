package com.github.retronym.macrocosm

object MacrocosmTest extends App {
  import Macrocosm._

  val s = desugar(List(1, 2, 3).reverse)
  println(s) // immutable.this.List.apply[Int](1, 2, 3).reverse

  try {
    assert1("boo".reverse == "obb")
  } catch {
    case a: AssertionError => println(a.getMessage) // scala.this.Predef.augmentString("boo").reverse.==("obb")
  }

  val b: Boolean = log("".isEmpty) // prints "".isEmpty() = true

  println(b"0101010") // 42

  // regex("a{") does not compile
  regex("a{1}")

  def plus(a: Int, b: Int) = a + b

  val i: Int = trace(plus(1, plus(2, 3)))

  trace("foo".toString.toString)

  trace(List(1, 2).reverse.reverse)

  val as = Array(1, 2, 3)

  arrayForeachWithIndex(as)((a, i) => println((a, i)))

  {var i = 0; cfor(0)(_ < 10, _ + 1)((a: Int) => i += 1)}

  iteratorForeach(Iterator(1, 2, 3, 4, 5))(println(_))

  case class Thing[A](a: A, b: Int)
  val thingA = lens[Thing[String]].a
  println(thingA._1(new Thing("foo", 1))) // foo
  println(thingA._2(new Thing("foo", 1), "bar")) // Thing("bar")

  // lens[Thing[String]].xxx // error: value xxx is not a member of Thing[String]

}
