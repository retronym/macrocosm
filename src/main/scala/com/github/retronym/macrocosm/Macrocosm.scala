package com.github.retronym.macrocosm

object Macrocosm {
  def macro showTree(a: Any): String = {
	  val s = show(a: Tree)
	  Literal(Constant(s))
	}

	def macro assert(c: Boolean) = {
		val assert = Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName("assert"))
		Apply(assert, List[Tree](c, Literal(Constant(show(c)))))
	}
}