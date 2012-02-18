package com.github.retronym.macrocosm

object MacrocosmTest extends App {
	import Macrocosm.{showTree, assert}

  val s = showTree(List(1, 2, 3).reverse) 
  println(s) // immutable.this.List.apply[Int](1, 2, 3).reverse

  try {
    assert("boo".reverse == "obb")	
  } catch {
  	case a: AssertionError => a.getMessage // scala.this.Predef.augmentString("boo").reverse.==("obb")
  }
}
