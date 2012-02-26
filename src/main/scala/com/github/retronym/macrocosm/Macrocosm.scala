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
  def macro log[A](a: A): A = {
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

  implicit def enrichStringContext(sc: StringContext) = new RichStringContext(sc)

  class RichStringContext(sc: StringContext) {
    // This is how a non-macro version would be implemented.
    // def b() = {
    //   val s = sc.parts.mkString
    //   parseBinary(s).getOrElse(sys.error("invalid binary literal: " + s))
    // }

    /** Binary literal integer
     *
     *  {{{
     *  scala> b"101010"
     *  res0: Int = 42
     *  }}}
     */
    def macro b(): Int = {
      def parseBinary(s: String): Option[Int] = {
        var i = s.length - 1
        var sum = 0
        var mult = 1
        while (i >= 0) {
          s.charAt(i) match {
            case '1' => sum += mult
            case '0' =>
            case x => return None
          }
          mult *= 2
          i -= 1
        }
        Some(sum)
      }

      val i = _this match {
        // e.g: `c.g.r.m.Macrocosm.enrichStringContext(scala.StringContext.apply("1111"))`
        case Apply(_, List(Apply(_, List(Literal(Constant(const: String)))))) =>
          parseBinary(const)
        case _ =>
          sys.error("Unexpected tree: " + show(_this))
      }
      i.map(x => Literal(Constant(x))).getOrElse(sys.error("invalid binary literal"))
    }
  }

  /**
   * Statically checked version of `"some([Rr]egex)".r`.
   * Invalid regular expressions trigger a compile failure.
   * At runtime, the regex is parsed again.
   *
   * {{{
   *  scala> regex(".*")
   * res0: scala.util.matching.Regex = .*
   *
   * scala> regex("{")
   * <console>:11: error: exception during macro expansion: Illegal repetition
   * {
   *           regex("{")
   *                ^
   * }}}
   */
  def macro regex(s: String): scala.util.matching.Regex = {
      s match {
        case Literal(Constant(string: String)) =>
          string.r // just to check
        Select(s, "r")
      }
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

  def macro tree(a: Any) = {
    reify(a)
  }

  implicit def infixNumericOps[T](x: T)(implicit num: Numeric[T]): Ops[T] = new Ops[T](x)

  class Ops[T](lhs: T)(implicit T: Numeric[T]) {
    import T._

    def macro +(rhs: T) = Util(_context).binaryNumericOp(_this, "plus", rhs)

    def macro -(rhs: T) = Util(_context).binaryNumericOp(_this, "minus", rhs)

    def macro *(rhs: T) = Util(_context).binaryNumericOp(_this, "times", rhs)

    def macro unary_-() = Util(_context).unaryNumericOp(_this, "negate")

    def macro abs() = Util(_context).unaryNumericOp(_this, "abs")

    def macro signum() = Util(_context).unaryNumericOp(_this, "signum")

    def macro toInt() = Util(_context).unaryNumericOp(_this, "toInt")

    def macro toLong() = Util(_context).unaryNumericOp(_this, "toLong")

    def macro toFloat() = Util(_context).unaryNumericOp(_this, "toFloat")

    def macro toDouble() = Util(_context).unaryNumericOp(_this, "toDouble")
  }

  /**
   * Converts:
   * {{{
   *  iteratorForeach(iterator)(a => <body>)
   * }}}
   *
   * To:
   * {{{
   * while(iterator.hasNext) f(iterator.next())
   * }}}
   * where `f` is inlined.
   */
  def macro iteratorForeach[A](iterator: Iterator[A])(f: A => Unit) = {
    val util = Util(_context); import util._

    val b = Block(
      List(
        ValDef(
          Modifiers(),
          newTermName("is"),
          TypeTree(),
          iterator
        )
      ),
      LabelDef(
        newTermName("while$1"),
        List(),
        If(
          Select(Ident(newTermName("is")), newTermName("hasNext")),
          Block(
            List(
              Block(
                List(
                  ValDef(
                    Modifiers(),
                    newTermName("i"),
                    TypeTree(),
                    Apply(Select(Ident(newTermName("is")), newTermName("next")), List())
                  )
                ),
                inlineApply(f, List(Ident(newTermName("i"))))
              )
            ),
            Apply(Ident(newTermName("while$1")), List())
          ),
          Literal(Constant(()))
        )
      )
    )
    resetAllAttrs(b)
  }

  /**
   * Fast, indexed, foreach over an array, translated to a while loop.
   *
   * {{{
   *  arrayForeachWithIndex(as)((a, i) => println((a, i)))
   * }}}
   *
   * Translated to:
   * {{{
   * {
   *  val $array = as
   *  var $i = 0
   *  val $len = array.length
   *  while ($i < $len) {
   *    val $a = array.apply($i)
   *    f($a, $i);
   *    $i += 1
   *  }
   * }
   * }}}
   * where the `f` is inlined.
   */
  def macro arrayForeachWithIndex[A](array: Array[A])(f: (A, Int) => Unit) = {
    val util = Util(_context); import util._

    val b = Block(
      List(
        ValDef(
          Modifiers(),
          newTermName("$array"),
          TypeTree(),
          array
        ),
        ValDef(
          Modifiers(Set(reflect.api.Modifier.mutable)),
          newTermName("$i"),
          TypeTree(),
          Literal(Constant(0))
        ),
        ValDef(
          Modifiers(),
          newTermName("$len"),
          TypeTree(),
          Select(Ident(newTermName("$array")), newTermName("length"))
        )
      ),
      LabelDef(
        newTermName("while$1"),
        List(),
        If(
          Apply(Select(Ident(newTermName("$i")), newTermName("$less")), List(Ident(newTermName("$len")))),
          Block(
            List(
              Block(
                List(
                  ValDef(
                    Modifiers(),
                    newTermName("$a"),
                    TypeTree(),
                    Apply(Select(Ident(newTermName("$array")), newTermName("apply")), List(Ident(newTermName("$i"))))
                  ),
                  inlineApply(f, List(Ident(newTermName("$a")), Ident(newTermName("$i"))))
                ),
                Assign(Ident(newTermName("$i")), Apply(Select(Ident(newTermName("$i")), newTermName("$plus")), List(Literal(Constant(1)))))
              )
            ),
            Apply(Ident(newTermName("while$1")), List())
          ),
          Literal(Constant(()))
        )
      )
    )
    resetAllAttrs(b)
  }

  /**
   * This call:
   * {{{
   * cfor(zero = 0)(okay = _ < 10, next = _ += 2) { println(_) }
   * }}}
   *
   * Translates to:
   * {{{
   * val a = zero
   * while (okay(a)) {
   *   act(a)
   *   a = next(a)
   * }
   * }}}
   * where the bodies of `okay`, `next`, and `act` are inlined.
   */
  // Suggested by Rex Kerr here: http://www.scala-lang.org/node/9809
  def macro cfor[A](zero: A)(okay: A => Boolean, next: A => A)(act: A => Unit) = {
    val util = Util(_context); import util._

    val elementVarName = newTermName("$a")

    val b = Block(
      List(
        ValDef(
          Modifiers(Set(reflect.api.Modifier.mutable)),
          elementVarName,
          TypeTree(),
          zero
        )
      ),
      LabelDef(
        newTermName("while$1"),
        List(),
        If(
          // okay(a)
          inlineApply(okay, List(Ident(elementVarName))),
          Block(
            List(
              Block(
                List(
                  // act(a)
                  inlineApply(act, List(Ident(elementVarName)))
                ),
                // a = next(a)
                Assign(
                  Ident(elementVarName),
                  inlineApply(next, List(Ident(elementVarName)))
                )
              )
            ),
            Apply(Ident(newTermName("while$1")), List())
          ),
          Literal(Constant(()))
        )
      )
    )
    resetAllAttrs(b)
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

    def literalFunctionToLocalMethod(f: Tree, methodName: TermName): Tree = {
      f match {
          case Function(params, body)  =>
            DefDef(Modifiers(), methodName, List(), List(params), TypeTree(), body)
          case _ =>  sys.error("parameter `f` must be a function literal.")
      }
    }

    /**
     * In:
     * `((p1, p2, ... pN) => <body>).apply(a1, a2, ..., aN)`
     *
     * Out:
     * ```
     * val p1 = a1; val p2 = a2; ... val pN = aN;
     * <body>
     * ````
     */
    def inlineApply(f: Tree, args: List[Tree]): Tree = {
      f match {
          case Function(params, body)  =>
            if (params.length != args.length) sys.error("incorrect arity")
            // val a = args(0); val b = args(1); ...
            val paramVals = params.zip(args).map {
              case (ValDef(_, pName, _, _), a) =>
                ValDef(Modifiers(), pName, TypeTree(), a)
            }
            Block(paramVals, body)
          case _ =>  sys.error("parameter `f` must be a function literal.")
      }
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

    def withNumericAndLhs(_this: Tree)(f: (Tree, Tree) => Tree) = {
      _this match {
        case Apply(Apply(TypeApply(_ /*infixNumericOps*/, _), List(lhs)), List(numeric)) =>
          f(numeric, lhs)
        case t => sys.error("unexpected tree: " + show(t))
      }
    }

    def unaryNumericOp(_this: Tree, methodName: String) = withNumericAndLhs(_this) {
      (numeric, lhs) =>
        Apply(Select(numeric, newTermName(methodName)), List(lhs))
    }

    def binaryNumericOp(_this: Tree, methodName: String, rhs: Tree) = withNumericAndLhs(_this) {
      (numeric, lhs) =>
        Apply(Select(numeric, newTermName(methodName)), List(lhs, rhs))
    }
  }
}