package com.github.retronym.macrocosm

object MacrocosmTest extends App {
	import Macrocosm._

  val s = showTree(List(1, 2, 3).reverse) 
  println(s) // immutable.this.List.apply[Int](1, 2, 3).reverse

  try {
    assert1("boo".reverse == "obb")	
  } catch {
  	case a: AssertionError => println(a.getMessage) // scala.this.Predef.augmentString("boo").reverse.==("obb")
  }

  try {
    assert2("boo".reverse == "obb"); 
  } catch {
  	case a: AssertionError => println(a.getMessage) // assert2("boo".reverse == "obb")
  }
}
