package com.github.retronym.macrocosm

import scala.reflect.makro.Context

object Macrocosm {
  /**
   * @return the tree of `a` after the typer, printed as source code.
   */
  def desugar(a: Any): String = macro desugarImpl

  def desugarImpl(c: Context)(a: c.Expr[Any]) = {
    import c.mirror._

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
    import c.mirror._
    val condCode = c.Expr[String](Literal(Constant(show(cond))))
    c.reify {
      assert(cond.eval, condCode.eval)
      ()
    }
  }

  /**
   * ```
   * log("".isEmpty) // prints: "".isEmpty = true
   * ```
   */
  def log[A](a: A): A = macro logImpl[A]

  def logImpl[A: c.TypeTag](c: Context)(a: c.Expr[A]): c.Expr[A] = {
    import c.mirror._
    val aCode = c.Expr[String](Literal(Constant(show(a))))
    c.reify {
      val temp = a.eval  // replace with `a.value` when that works.
      println(aCode.eval + " = " + temp)
      temp
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

    import c.mirror._

    val i = c.prefix.tree match {
      // e.g: `c.g.r.m.Macrocosm.enrichStringContext(scala.StringContext.apply("1111"))`
      case Apply(_, List(Apply(_, List(Literal(Constant(const: String)))))) =>
        parseBinary(const)
      case x =>
        sys.error("Unexpected tree: " + show(x))
    }
    c.Expr[Int](Literal(Constant(i.getOrElse(sys.error("invalid binary literal")))))
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
      import c.mirror._

      s.tree match {
        case Literal(Constant(string: String)) =>
          string.r // just to check
          reify(s.eval.r)
      }
  }

  /**
   * Trace execution on `c`, by printing the values of sub-expressions
   * to standard out.
   */
  def trace[A](expr: A) = macro traceImpl[A]

  def traceImpl[A: c.TypeTag](c: Context)(expr: c.Expr[A]): c.Expr[A] = {
    import c.mirror._

    // stand-in for the real type of the traced sub expression.
    // This lets us conveniently splice this type into the
    // reify call. The correct type is passed in implicitly
    // as the TypeTag[T]
    trait T

    // TODO:  an unexplained "forward reference" error occurs
    //        if this is moved below and turned into a val.
    implicit var TTag: TypeTag[T] = null

    object tracingTransformer extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case a @ Apply(qual, args) =>
            val sub = Apply(transform(qual), args.map(a => transform(a)))
            val subExpr = Expr[T](sub)
            val subExprCode = Expr[String](Literal(Constant(show(a))))
            TTag = TypeTag(sub.tpe)
            reify {
              val temp: T = subExpr.eval
              println("" + subExprCode.eval + " = " + temp)
              temp
            }
          case a @ Select(qual, name) if name.isTermName =>
            val sub = Select(transform(qual), name)
            a.tpe match {
              case MethodType(_, _) | PolyType(_, _) =>
                // qual.meth(...)
                // \-------/
                //    don't trace this part.
                sub
              case _ =>
                val subExpr = Expr[T](sub)
                val subExprCode = Expr[Any](Literal(Constant(show(a))))
                TTag = TypeTag(sub.tpe)
                reify {
                  val temp: T = subExpr.eval
                  println("" + subExprCode.eval + " = " + temp)
                  temp
                }
            }
          case _ => super.transform(tree)
        }
      }
    }
    val t = tracingTransformer.transform(expr.tree)
    Expr[A](c.resetAllAttrs(t))
  }

  implicit def infixNumericOps[T](x: T)(implicit num: Numeric[T]): NumericOps[T] = new NumericOps[T](x)

  class NumericOps[T](lhs: T)(implicit T: Numeric[T]) {
    def +(rhs: T) = macro NumericOps.+[T]
    def -(rhs: T) = macro NumericOps.-[T]
    def *(rhs: T) = macro NumericOps.*[T]
    def unary_-() = macro NumericOps.unary_-[T]
    def abs() = macro NumericOps.abs[T]
    def signum() = macro NumericOps.signum[T]
    def toInt() = macro NumericOps.toInt[T]
    def toLong() = macro NumericOps.toLong[T]
    def toDouble() = macro NumericOps.toDouble[T]
  }

  object NumericOps {
    def +[T](c: Context)(rhs: c.Expr[T]) = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.plus(lhs.eval, rhs.eval))
    }

    def -[T](c: Context)(rhs: c.Expr[T]) = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.minus(lhs.eval, rhs.eval))
    }

    def *[T](c: Context)(rhs: c.Expr[T]) = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.times(lhs.eval, rhs.eval))
    }

    def unary_-[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.negate(lhs.eval))
    }

    def abs[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.abs(lhs.eval))
    }

    def signum[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.signum(lhs.eval))
    }

    def toInt[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.toInt(lhs.eval))
    }

    def toLong[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.toLong(lhs.eval))
    }

    def toDouble[T](c: Context)() = {
      val (numeric, lhs) = extractNumericAndLhs[T](c)
      c.reify(numeric.eval.toDouble(lhs.eval))
    }

    def extractNumericAndLhs[T](c: Context): (c.Expr[Numeric[T]], c.Expr[T]) = {
      import c.mirror._

      c.prefix.tree match {
        case Apply(Apply(TypeApply(_ /*infixNumericOps*/, _), List(lhs)), List(numeric)) =>
          (c.Expr(numeric), c.Expr(lhs))
        case t => sys.error("unexpected tree: " + show(t))
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

  def iteratorForeachImpl[A: c.TypeTag]
                         (c: Context)
                         (iterator: c.Expr[Iterator[A]])
                         (act: c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.mirror._
    val util = Util(c)
    val elementVarName = newTermName("$elem")
    val actExpr = Expr[Unit](util.inlineApply(act, List(Ident(elementVarName))))

    val e = reify {
      val i = iterator.eval
      while(i.hasNext) {
        val $elem = i.next()
        actExpr.eval
      }
    }
    c.Expr[Unit](c.resetAllAttrs(e.tree))
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

  def arrayForeachWithIndexImpl[A: c.TypeTag]
                               (c: Context)
                               (array: c.Expr[Array[A]])
                               (f: c.Expr[(A, Int) => Unit]): c.Expr[Unit] = {
    import c.mirror._
    val util = Util(c); import util._
    val indexVarName = newTermName("$i")
    val elementVarName = newTermName("$elem")
    val fExpr = Expr[Unit](util.inlineApply(f, List(Ident(elementVarName), Ident(indexVarName))))

    val expr = reify {
      val a = array.eval
      var $i = 0
      val len = a.length
      while ($i < len) {
        val $elem = a($i)
        fExpr.eval
        $i += 1
      }
    }

    Expr(c.resetAllAttrs(expr.tree))
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

  def cforImpl[A: c.TypeTag]
              (c: Context)
              (zero: c.Expr[A])
              (okay: c.Expr[A => Boolean], next: c.Expr[A => A])
              (act: c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.mirror._
    val util = Util(c)

    val elementVarName = newTermName("$elem")
    def inlineApplyExpr[B](f: c.Expr[A => B]) =
      Expr[B](util.inlineApply(f, List(Ident(elementVarName))))

    val okayInline = inlineApplyExpr[Boolean](okay)
    val nextInline = inlineApplyExpr[A](next)
    val actInline = inlineApplyExpr[Unit](act)

    val t = reify {
      var $elem: A = zero.eval
      while(okayInline.eval) {
        actInline.eval
        $elem = nextInline.eval
      }
    }
    c.Expr[Unit](c.resetAllAttrs(t))
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
    def applyDynamic(propName: String)(dummy: Any)
                     = macro Lenser.applyDynamic[T]
  }

  //
  // the `dummy` parameter is a workaround for bug in macros.
  //
  // scala> object M { def m(a: String)() = macro mImpl[Int]; def mImpl[A: c.TypeTag](c: reflect.makro.Context)(a: c.Expr[String])() = a }
  // defined module M
  //
  // scala> M.m("foo")()<console>:12: error: exception during macro expansion: assertion failed:
  //
  object Lenser {
    def applyDynamic[T: c.TypeTag]
                    (c: Context)
                    (propName: c.Expr[String])
                    (dummy: c.Expr[Any])
                     = {
      import c.mirror._
      val util = Util(c); import util._
      import reflect.api.Modifier
      //println(showRaw(_this))
      val t = (c.prefix.tree, propName.tree) match {
        case (TypeApply(
          Select(
            _, lensMethodTermName
          ), List(tpe)), Literal(Constant(methodName: String))) =>
          val getterMember = tpe.tpe.member(newTermName(methodName))
          if (getterMember == NoSymbol) sys.error("value " + methodName + " is not a member of " + tpe.tpe)
          val memberType = getterMember.typeSignatureIn(tpe.tpe) match {
            case NullaryMethodType(memberType) => memberType
            case _ => sys.error("member %s is not a field".format(methodName))
          }
          val getter = Function(List(ValDef(Modifiers(Set(Modifier.parameter)), newTermName("a$"), TypeTree().setType(tpe.tpe), EmptyTree)), Select(Ident(newTermName("a$")), newTermName(methodName)))
          val setter = Function(
            List(
              ValDef(Modifiers(Set(Modifier.parameter)), newTermName("a$"), TypeTree().setType(tpe.tpe), EmptyTree),
              ValDef(Modifiers(Set(Modifier.parameter)), newTermName("x$"), TypeTree().setType(memberType), EmptyTree)
            ),
            Apply(Select(Ident(newTermName("a$")), newTermName("copy")), List(AssignOrNamedArg(Ident(newTermName(methodName)), Ident(newTermName("x$")))))
          )
          val getterSetter = Apply(Select(Select(Ident(newTermName("scala")), newTermName("Tuple2")), newTermName("apply")), List(getter, setter))
          getterSetter
        case x => sys.error("unexpected c.prefix tree: " + x)
      }
      c.Expr[Any](c.resetAllAttrs(t))
    }
  }

  implicit def Util(context: Context) = new Util[context.type](context)

  class Util[C <: Context with Singleton](val context: C) {
    import context.mirror._

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
  }
}