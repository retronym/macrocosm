package com.github.retronym.macrocosm

object Macrocosm {
  def macro showTree(a: Any): String = {
	  val s = show(a: Tree)
	  Literal(Constant(s))
	}

	def macro assert1(c: Boolean) = {
		implicit val __context: _context.type = _context

		Apply(predefAssert, List[Tree](c, Literal(Constant(show(c)))))
	}

	def macro assert2(c: Boolean) = {
		implicit val __context: _context.type = _context
		val cTree: Tree = c
		val pos = c.pos.asInstanceOf[scala.tools.nsc.util.OffsetPosition]

		Apply(predefAssert, List[Tree](c, Literal(Constant(pos.lineContent.trim))))
	}

	private[this] def predefAssert(implicit context: scala.reflect.macro.Context): context.Tree = {
		import context._
		Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName("assert"))
	}
}