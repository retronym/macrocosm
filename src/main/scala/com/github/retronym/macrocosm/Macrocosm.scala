package com.github.retronym.macrocosm

object Macrocosm {
  def macro showTree(a: Any): String = {
    val s = show(a: Tree)
    Literal(Constant(s))
  }

  def macro assert1(c: Boolean) = {
    implicit val __context: _context.type = _context

    Apply(predefAssert, List[Tree](c, stringLit(show(c))))
  }

  def macro assert2(c: Boolean) = {
    implicit val __context: _context.type = _context
    val cTree: Tree = c
    val pos = c.pos.asInstanceOf[scala.tools.nsc.util.OffsetPosition]

    Apply(predefAssert, List[Tree](c, stringLit(pos.lineContent.trim)))
  }

  private var count = 0
  def nextName() = {
    count += 1
    "i$" + count
  }


  def macro trace(c: Any) = {
    implicit val __context: _context.type = _context
    object tracingTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case a @ Apply(qual, args) =>        
          val tempValName = newTermName(nextName)
          val sub = Apply(transform(qual), args.map(a => transform(a)))
          Block (            
            List(
              ValDef(Modifiers(), tempValName, TypeTree(sub.tpe), sub),            
              Apply(predefPrint, List(stringLit(show(a) + " = "))),
              Apply(predefPrintln, List(Ident(tempValName)))
            ),
            Ident(tempValName)
          )
        case _ => super.transform(tree)
      }
    }
    val t = tracingTransformer.transform(c)
    //println("t = " + t)
    t
  }
  
  private[this] def predefAssert(implicit context: scala.reflect.macro.Context): context.Tree =
    predefSelect("assert")

  private[this] def predefPrintln(implicit context: scala.reflect.macro.Context): context.Tree =
    predefSelect("println")

  private[this] def predefPrint(implicit context: scala.reflect.macro.Context): context.Tree =
    predefSelect("print")

  private[this] def predefSelect(name: String)(implicit context: scala.reflect.macro.Context): context.Tree = {
    import context._
    Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName(name))
  }

  private[this] def stringLit(s: String)(implicit context: scala.reflect.macro.Context): context.Tree = {
    import context._
    Literal(Constant(s))
  }
}