package com.github.retronym.macrocosm

object Macrocosm {
  /**
   * @return the tree of `a` after the typer, printed as source code.
   */
  def macro desugar(a: Any): String = {
    val util = Util(_context); import util._    

    val s = show(a: Tree)
    stringLit(s)
  }

  /**
   * Assert that `c` is true. The tree of `c` is used as the assertion message.
   */
  def macro assert1(c: Boolean) = {
    val util = Util(_context); import util._

    Apply(predefAssert, List[Tree](c, stringLit(show(c))))
  }

  /**
   * ```
   * log("".isEmpty) // prints: "".isEmpty = true
   * ```
   */
  def macro log(a: Any) = {
    val util = Util(_context); import util._
    val tempValName = newTermName("$value")
    Block(
      List(
        ValDef(Modifiers(), tempValName, TypeTree(), a),       
        Apply(predefPrint, List(stringLit(show(a) + " = "))),
        Apply(predefPrintln, List(Ident(tempValName)))
      ),
      Ident(tempValName)
    )
  }

  /**
   * Assert that `c` is true. The line of source code from the caller is used
   * as the assertion message.
   */
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


  /**
   * Trace execution on `c`, by printing the values of sub-expressions
   * to standard out.
   */
  def macro trace(c: Any) = {
    val util = Util(_context); import util._    
    object tracingTransformer extends Transformer {
      //val symTree = mutable.buffer[SymTree]
      override def transform(tree: Tree): Tree = {        
        tree match {
          case a @ Apply(qual, args) =>
            val tempValName = newTermName(nextName)            
            val sub = Apply(transform(qual), args.map(a => transform(a)))
            Block(
              List(
                ValDef(Modifiers(), tempValName, TypeTree(), sub),       
                Apply(predefPrint, List(stringLit(show(a) + " = "))),
                Apply(predefPrintln, List(Ident(tempValName)))
              ),
              Ident(tempValName)
            )
          case a @ Select(qual, name) if name.isTermName =>
            val tempValName = newTermName(nextName)                        
            val sub = Select(transform(qual), name)
            a.tpe match {
              case MethodType(_, _) | PolyType(_, _) => 
                // qual.meth(...)
                // \-------/
                //    don't trace this part.
                sub
              case _ => 
                Block(
                  List(
                    ValDef(Modifiers(), tempValName, TypeTree(), sub),       
                    Apply(predefPrint, List(stringLit(show(a) + " = "))),
                    Apply(predefPrintln, List(Ident(tempValName)))
                  ),
                  Ident(tempValName)
                )
            }            
          case _ => super.transform(tree)
        }
      }
    }
    val t = tracingTransformer.transform(c)
    //println("t = " + t)
    resetAllAttrs(t)
  }
  

  import scala.reflect.macro.Context

  implicit def Util(context: Context) = new Util[context.type](context)

  class Util[C <: Context with Singleton](val context: C) {
    import context._
    
    def id(a: Tree): Tree = a 

    // The first version of the trace macro did not call this, which led to NSDNHO
    // errors with `trace(1.toString.toString)`.
    //
    // Explanation from Eugene:
    //
    // The trouble was with the fact that the tree produced by trace(1.toString.toString)
    // was partially typed. And when some AST already has a type, typer doesn't drill into
    // its children and just moves along. Consequently, newly generated valdefs were never
    // processed by the typer, i.e. never got symbols assigned to them, hence the NSDNHO.
    def resetAllAttrs(a: Tree): Tree = {
      val global = context.asInstanceOf[scala.tools.nsc.Global]
      global.resetAllAttrs(a.asInstanceOf[global.Tree])
             .asInstanceOf[Tree]
    }
     
    def predefAssert: Tree =
      predefSelect("assert")

    def predefPrintln: Tree =
      predefSelect("println")

    def predefPrint: Tree =
      predefSelect("print")

    def predefSelect(name: String): Tree =
      Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName(name))

    def stringLit(s: String): Tree =
      Literal(Constant(s))
  }
}