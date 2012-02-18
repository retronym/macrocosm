package com.github.retronym.macrocosm

object Macrocosm {
  def macro showTree(a: Any): String = {
	  val s = show(a: Tree)
	  Literal(Constant(s))
	}
}