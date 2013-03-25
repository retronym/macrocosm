package com.github.retronym.macrocosm

import scala.reflect.macros.Context
import language.experimental.macros
import language.dynamics

object Macrocosm {
  /**
   * @return the tree of `a` after the typer, printed as source code.
   */
  def desugar(a: Any): String = macro desugarImpl

  def desugarImpl(c: Context)(a: c.Expr[Any]) = {
    import c.universe._

    val s = show(a.tree)
    c.Expr(
      Literal(Constant(s))
    )
  }

  /**
   * Assert that `cond` is true. The tree of `cond` is used as the assertion message.
   */
  def assert1(cond: Boolean): Unit = macro assert1Impl

  def assert1Impl(c: Context)(cond: c.Expr[Boolean]) = {
    import c.universe._
    val condCode = c.Expr[String](Literal(Constant(show(cond.tree))))
    c.universe.reify {
      assert(cond.splice, condCode.splice)
      ()
    }
  }

  /**
   * ```
   * log("".isEmpty) // prints: "".isEmpty = true
   * ```
   */
  def log[A](a: A): A = macro logImpl[A]

  def logImpl[A: c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val aCode = c.Expr[String](Literal(Constant(show(a.tree))))
    c.universe.reify {
      val result = a.splice
      println(aCode.splice + " = " + result)
      result
    }
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
    def b(): Int = macro bImpl
  }

  def bImpl(c: Context)(): c.Expr[Int] = {
    def parseBinary(s: String): Int = {
      var i = s.length - 1
      var sum = 0
      var mult = 1
      while (i >= 0) {
        s.charAt(i) match {
          case '1' => sum += mult
          case '0' =>
          case x =>
            c.abort(c.enclosingPosition, "invalid binary literal")
        }
        mult *= 2
        i -= 1
      }
      sum
    }

    import c.universe._

    val i = c.prefix.tree match {
      // e.g: `c.g.r.m.Macrocosm.enrichStringContext(scala.StringContext.apply("1111"))`
      case Apply(_, List(Apply(_, List(Literal(Constant(const: String)))))) =>
        parseBinary(const)
      case x =>
        c.abort(c.enclosingPosition, "unexpected tree: " + show(x))
    }
    c.Expr[Int](Literal(Constant(i)))
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
  def regex(s: String): scala.util.matching.Regex = macro regexImpl

  def regexImpl(c: Context)(s: c.Expr[String]): c.Expr[scala.util.matching.Regex] = {
    import c.universe._

    s.tree match {
      case Literal(Constant(string: String)) =>
        string.r // just to check
        c.universe.reify(s.splice.r)
    }
  }

  /**
   * Trace execution on `c`, by printing the values of sub-expressions
   * to standard out.
   */
  def trace[A](expr: A) = macro traceImpl[A]

  def traceImpl[A: c.WeakTypeTag](c: Context)(expr: c.Expr[A]): c.Expr[A] = {
    import c.universe._

    object tracingTransformer extends Transformer {
      def insertTrace(t: Tree): Tree = {
        val expr     = c.Expr[Any](t)
        val exprCode = c.Expr[String](Literal(Constant(show(t))))
        val exprTpe  = c.Expr[String](Literal(Constant(show(t.tpe))))

        (c.universe.reify {
          val result = expr.splice
          println("%s = %s: %s".format(exprCode.splice, result, exprTpe.splice))
          result
        }).tree
      }

      override def transform(tree: Tree): Tree = {
        tree match {
          case Apply(_, _)               =>
            val result = super.transform(tree)
            insertTrace(tree)
          case Select(_, _: TermName) =>
            val result = super.transform(tree)
            // qual.meth(...)
            // \-------/
            //    don't trace this part.
            result.tpe match {
              case MethodType(_, _) | PolyType(_, _) => result
              case _                                 => insertTrace(tree)
            }
          case _ =>
            super.transform(tree)
        }
      }
    }
    val t = tracingTransformer.transform(expr.tree)
    c.Expr[A](c.resetAllAttrs(t))
  }

  implicit def infixNumericOps[T](x: T)(implicit num: Numeric[T]): NumericOps[T] = new NumericOps[T](x)

  class NumericOps[T](lhs: T)(implicit T: Numeric[T]) {
    def +(rhs: T)  = macro NumericOps.+[T]
    def -(rhs: T)  = macro NumericOps.-[T]
    def *(rhs: T)  = macro NumericOps.*[T]
    def unary_-()  = macro NumericOps.unary_-[T]
    def abs()      = macro NumericOps.abs[T]
    def signum()   = macro NumericOps.signum[T]
    def toInt()    = macro NumericOps.toInt[T]
    def toLong()   = macro NumericOps.toLong[T]
    def toDouble() = macro NumericOps.toDouble[T]
  }

  object NumericOps {
    def +[T](c: Context)(rhs: c.Expr[T]) = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.plus(lhs.splice, rhs.splice))
    }

    def -[T](c: Context)(rhs: c.Expr[T]) = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.minus(lhs.splice, rhs.splice))
    }

    def *[T](c: Context)(rhs: c.Expr[T]) = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.times(lhs.splice, rhs.splice))
    }

    def unary_-[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.negate(lhs.splice))
    }

    def abs[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.abs(lhs.splice))
    }

    def signum[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.signum(lhs.splice))
    }

    def toInt[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.toInt(lhs.splice))
    }

    def toLong[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.toLong(lhs.splice))
    }

    def toDouble[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.universe.reify(numeric.splice.toDouble(lhs.splice))
    }

    def extractNumericAndLhs[T](c: Context): (c.Expr[Numeric[T]], c.Expr[T]) = {
      import c.universe._

      c.prefix.tree match {
        case Apply(Apply(TypeApply(_ /*infixNumericOps*/, _), List(lhs)), List(numeric)) =>
          (c.Expr(numeric), c.Expr(lhs))
        case t =>
          c.abort(c.enclosingPosition, "unexpected tree: " + show(t))
      }
    }
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
  def iteratorForeach[A](iterator: Iterator[A])
                        (act: A => Unit): Unit =
    macro iteratorForeachImpl[A]

  def iteratorForeachImpl[A: c.WeakTypeTag]
                         (c: Context)
                         (iterator: c.Expr[Iterator[A]])
                         (act: c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._

    val e = c.universe.reify {
      val i = iterator.splice
      while(i.hasNext) {
        val elem = i.next()
        act.splice(elem)
      }
    }
    c.inlineAndReset(e)
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
  def arrayForeachWithIndex[A](array: Array[A])(f: (A, Int) => Unit): Unit =
    macro arrayForeachWithIndexImpl[A]

  def arrayForeachWithIndexImpl[A: c.WeakTypeTag]
                               (c: Context)
                               (array: c.Expr[Array[A]])
                               (f: c.Expr[(A, Int) => Unit]): c.Expr[Unit] = {
    import c.universe._

    val expr = c.universe.reify {
      val a = array.splice
      var i = 0
      val len = a.length
      while (i < len) {
        val elem = a(i)
        f.splice(elem, i)
        i += 1
      }
    }

    c.inlineAndReset(expr)
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
  def cfor[A](zero: A)(okay: A => Boolean, next: A => A)(act: A => Unit): Unit =
    macro cforImpl[A]

  def cforImpl[A: c.WeakTypeTag]
              (c: Context)
              (zero: c.Expr[A])
              (okay: c.Expr[A => Boolean], next: c.Expr[A => A])
              (act: c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._

    val t = c.universe.reify {
      var elem: A = zero.splice
      while(okay.splice(elem)) {
        act.splice(elem)
        elem = next.splice(elem)
      }
    }
    c.inlineAndReset(t)
  }

  // //case class Lens[A, B](getter: A => B, setter: (A, B) => A)

  /**
   * Automatic lens generation.
   *
   * {{{
   * case class Foo(a: Int)
   * val l = lens[Foo].a
   * val foo = Foo(0)
   * l._1(foo) // 0
   * l._2(foo, 1) // Foo(1)
   * }}}
   */
  def lens[T] = new Lenser[T]

  class Lenser[T] extends Dynamic {
    def selectDynamic(propName: String)  = macro Lenser.selectDynamic[T]
    def applyDynamic(propName: String)() = macro Lenser.applyDynamic[T]
  }

  object Lenser {
    def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) =
      applyDynamic[T](c)(propName)()

    def applyDynamic[T: c.WeakTypeTag]
                    (c: Context)
                    (propName: c.Expr[String])
                    ()
                     = {
      import c.universe._
      // Why doesn't this work if I try to use scala.Tuple2's symbol?
      def Tuple2Module = Select(Ident(newTermName("scala")), newTermName("Tuple2"))
      def mkParam(name: String, tpe: Type) =
        ValDef(Modifiers(Flag.PARAM), newTermName(name), TypeTree(tpe), EmptyTree)

      import treeBuild._
      //println(showRaw(_this))
      val t = (c.prefix.tree, propName.tree) match {
        case (TypeApply(
          Select(
            _, lensMethodTermName
          ), List(tpe)), Literal(Constant(methodName: String))) =>
          val getterMember = tpe.tpe.member(newTermName(methodName)) orElse {
            c.abort(c.enclosingPosition, "value " + methodName + " is not a member of " + tpe.tpe)
          }
          val memberType = getterMember.typeSignatureIn(tpe.tpe) match {
            case NullaryMethodType(memberType) => memberType
            case _                             => c.abort(c.enclosingPosition, "member %s is not a field".format(methodName))
          }
          val getter = Function(
            List(mkParam("a$", tpe.tpe)),
            Select(Ident(newTermName("a$")), newTermName(methodName))
          )
          val setter = Function(
            List(mkParam("a$", tpe.tpe), mkParam("x$", memberType)),
            Apply(
              Select(Ident(newTermName("a$")), newTermName("copy")),
              List(AssignOrNamedArg(Ident(newTermName(methodName)), Ident(newTermName("x$"))))
            )
          )
          mkMethodCall(Select(Tuple2Module, newTermName("apply")), List(getter, setter))
        case x =>
          c.abort(c.enclosingPosition, "unexpected c.prefix tree: " + x)
      }
      c.Expr[Any](c.resetAllAttrs(t))
    }
  }

  implicit def Util(context: Context) = new Util[context.type](context)

  class Util[C <: Context](val c: C) {
    import c.universe._

    def inlineAndReset[T](expr: c.Expr[T]): c.Expr[T] =
      c.Expr[T](c resetAllAttrs inlineApplyRecursive(expr.tree))

    /**
     * Reursively transforms `tree`, inlining direct function
     * application.
     *
     * In:
     * `((p1, p2, ... pN) => <body>).apply(a1, a2, ..., aN)`
     *
     * Out:
     * ```
     * val p1 = a1; val p2 = a2; ... val pN = aN;
     * <body>
     * ````
     */
    def inlineApplyRecursive(tree: Tree): Tree = {
      val ApplyName = newTermName("apply")

      object inliner extends Transformer {
        override def transform(tree: Tree): Tree = {
          tree match {
            case ap @ Apply(Select(prefix, ApplyName), args) =>
              prefix match {
                case Function(params, body)  =>
                  if (params.length != args.length)
                    c.abort(c.enclosingPosition, "incorrect arity: " + (params.length, args.length))
                  // val a$0 = args(0); val b$0 = args(1); ...
                  val paramVals = params.zip(args).map {
                    case (ValDef(_, pName, _, _), a) =>
                      ValDef(Modifiers(), newTermName("" + pName + "$0"), TypeTree(), a)
                  }
                  // val a = a$0; val b = b$0
                  val paramVals2 = params.zip(args).map {
                    case (ValDef(_, pName, _, _), a) =>
                      ValDef(Modifiers(), pName, TypeTree(), Ident(newTermName("" + pName + "$0")))
                  }
                  // The nested blocks avoid name clashes.
                  Block(paramVals, Block(paramVals2, body))
                case x => ap
              }
            case _ => super.transform(tree)
          }
        }
      }
      inliner.transform(tree)
    }
  }
}
