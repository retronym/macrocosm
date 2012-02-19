package com.github.retronym.macrocosm

object Macrocosm {
  def macro showTree(a: Any): String = {
    val util = Util(_context); import util._    

    val s = show(a: Tree)
    stringLit(s)
  }

  def macro assert1(c: Boolean) = {
    val util = Util(_context); import util._

    Apply(predefAssert, List[Tree](c, stringLit(show(c))))
  }

  def macro assert2(c: Boolean) = {
    val util = Util(_context); import util._    
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
    val util = Util(_context); import util._    
    object tracingTransformer extends Transformer {
      //val symTree = mutable.buffer[SymTree]
      override def transform(tree: Tree): Tree = {        
        tree match {
          case a @ Apply(qual, args) =>
            val tempValName = newTermName(nextName)
            //val tempValSymbol = newValue(tempValName)
            val sub = Apply(transform(qual), args.map(a => transform(a)))
            Block(
              List(
                new _context.ValDef(Modifiers(), tempValName, TypeTree(sub.tpe), sub),       
                Apply(predefPrint, List(stringLit(show(a) + " = "))),
                Apply(predefPrintln, List(Ident(tempValName)))
              ),
              Ident(tempValName)
            )
          case _ => super.transform(tree)
        }
      }
    }
    val t = tracingTransformer.transform(c)
    //println("t = " + t)
    t
  }
  

  import scala.reflect.macro.Context

  implicit def Util(context: Context) = new Util[context.type](context)

  class Util[C <: Context with Singleton](val context: C) {
    import context._
    
    def id(a: Tree): Tree = a 
     
    def predefAssert: Tree =
      predefSelect("assert")

    def predefPrintln: Tree =
      predefSelect("println")

    def predefPrint: Tree =
      predefSelect("print")

    def predefSelect(name: String): Tree = {
      import context._
      Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName(name))
    }

    def stringLit(s: String): Tree = {
      import context._
      Literal(Constant(s))
    }
  }
}