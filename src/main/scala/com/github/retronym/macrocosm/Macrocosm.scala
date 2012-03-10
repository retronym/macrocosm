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
    // c.reify(s)
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
            val tempValName = newTermName(nextName)
            val sub = Apply(transform(qual), args.map(a => transform(a)))
            val subExpr = Expr[T](sub)
            val subExprCode = Expr[String](Literal(Constant(show(a))))
            TTag = TypeTag(sub.tpe)
            reify {
              val temp: T = subExpr.eval
              println(subExprCode.eval + " = " + temp)
              temp
            }
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
                val subExpr = Expr[T](sub)
                val subExprCode = Expr[Any](Literal(Constant(show(a))))
                TTag = TypeTag(sub.tpe)
                reify {
                  val temp: T = subExpr.eval
                  println(subExprCode.eval + " = " + temp)
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

  // def macro tree(a: Any) = {
  //   val util = Util(_context); import util._
  //   reify(a)
  // }

  // def macro symbol[T](f: T => Unit) = {
  //   val util = Util(_context); import util._

  //   f match {
  //     case Function(List(_),
  //       Block(List(Apply(s @ Select(_, _), List())), _)) =>
  //       val AnyTpe = glb(List())
  //       Select(reify(Ident(s.symbol).setType(AnyTpe)), "symbol")
  //     case x => sys.error("unexpected tree: " + showRaw(x))
  //   }
  // }

  // implicit def infixNumericOps[T](x: T)(implicit num: Numeric[T]): Ops[T] = new Ops[T](x)

  // class Ops[T](lhs: T)(implicit T: Numeric[T]) {
  //   import T._

  //   def macro +(rhs: T) = Util(_context).binaryNumericOp(_this, "plus", rhs)

  //   def macro -(rhs: T) = Util(_context).binaryNumericOp(_this, "minus", rhs)

  //   def macro *(rhs: T) = Util(_context).binaryNumericOp(_this, "times", rhs)

  //   def macro unary_-() = Util(_context).unaryNumericOp(_this, "negate")

  //   def macro abs() = Util(_context).unaryNumericOp(_this, "abs")

  //   def macro signum() = Util(_context).unaryNumericOp(_this, "signum")

  //   def macro toInt() = Util(_context).unaryNumericOp(_this, "toInt")

  //   def macro toLong() = Util(_context).unaryNumericOp(_this, "toLong")

  //   def macro toFloat() = Util(_context).unaryNumericOp(_this, "toFloat")

  //   def macro toDouble() = Util(_context).unaryNumericOp(_this, "toDouble")
  // }

  // object Dyno extends Dynamic {
  //   val methods = Set("foo", "bar")
  //   def macro applyDynamic(mn: String)(args: Any*) = mn match {
  //     case Literal(Constant(x: String)) if methods(x) =>
  //       Literal(Constant(true))
  //     case _ => sys.error("no dice")
  //   }
  // }

  // /**
  //  * Converts:
  //  * {{{
  //  *  iteratorForeach(iterator)(a => <body>)
  //  * }}}
  //  *
  //  * To:
  //  * {{{
  //  * while(iterator.hasNext) f(iterator.next())
  //  * }}}
  //  * where `f` is inlined.
  //  */
  // def macro iteratorForeach[A](iterator: Iterator[A])(f: A => Unit) = {
  //   val util = Util(_context); import util._

  //   val b = Block(
  //     List(
  //       ValDef(
  //         Modifiers(),
  //         newTermName("is"),
  //         TypeTree(),
  //         iterator
  //       )
  //     ),
  //     LabelDef(
  //       newTermName("while$1"),
  //       List(),
  //       If(
  //         Select(Ident(newTermName("is")), newTermName("hasNext")),
  //         Block(
  //           List(
  //             Block(
  //               List(
  //                 ValDef(
  //                   Modifiers(),
  //                   newTermName("i"),
  //                   TypeTree(),
  //                   Apply(Select(Ident(newTermName("is")), newTermName("next")), List())
  //                 )
  //               ),
  //               inlineApply(f, List(Ident(newTermName("i"))))
  //             )
  //           ),
  //           Apply(Ident(newTermName("while$1")), List())
  //         ),
  //         Literal(Constant(()))
  //       )
  //     )
  //   )
  //   resetAllAttrs(b)
  // }

  // /**
  //  * Fast, indexed, foreach over an array, translated to a while loop.
  //  *
  //  * {{{
  //  *  arrayForeachWithIndex(as)((a, i) => println((a, i)))
  //  * }}}
  //  *
  //  * Translated to:
  //  * {{{
  //  * {
  //  *  val $array = as
  //  *  var $i = 0
  //  *  val $len = array.length
  //  *  while ($i < $len) {
  //  *    val $a = array.apply($i)
  //  *    f($a, $i);
  //  *    $i += 1
  //  *  }
  //  * }
  //  * }}}
  //  * where the `f` is inlined.
  //  */
  // def macro arrayForeachWithIndex[A](array: Array[A])(f: (A, Int) => Unit) = {
  //   val util = Util(_context); import util._

  //   val b = Block(
  //     List(
  //       ValDef(
  //         Modifiers(),
  //         newTermName("$array"),
  //         TypeTree(),
  //         array
  //       ),
  //       ValDef(
  //         Modifiers(Set(reflect.api.Modifier.mutable)),
  //         newTermName("$i"),
  //         TypeTree(),
  //         Literal(Constant(0))
  //       ),
  //       ValDef(
  //         Modifiers(),
  //         newTermName("$len"),
  //         TypeTree(),
  //         Select(Ident(newTermName("$array")), newTermName("length"))
  //       )
  //     ),
  //     LabelDef(
  //       newTermName("while$1"),
  //       List(),
  //       If(
  //         Apply(Select(Ident(newTermName("$i")), newTermName("$less")), List(Ident(newTermName("$len")))),
  //         Block(
  //           List(
  //             Block(
  //               List(
  //                 ValDef(
  //                   Modifiers(),
  //                   newTermName("$a"),
  //                   TypeTree(),
  //                   Apply(Select(Ident(newTermName("$array")), newTermName("apply")), List(Ident(newTermName("$i"))))
  //                 ),
  //                 inlineApply(f, List(Ident(newTermName("$a")), Ident(newTermName("$i"))))
  //               ),
  //               Assign(Ident(newTermName("$i")), Apply(Select(Ident(newTermName("$i")), newTermName("$plus")), List(Literal(Constant(1)))))
  //             )
  //           ),
  //           Apply(Ident(newTermName("while$1")), List())
  //         ),
  //         Literal(Constant(()))
  //       )
  //     )
  //   )
  //   resetAllAttrs(b)
  // }

  // /**
  //  * This call:
  //  * {{{
  //  * cfor(zero = 0)(okay = _ < 10, next = _ += 2) { println(_) }
  //  * }}}
  //  *
  //  * Translates to:
  //  * {{{
  //  * val a = zero
  //  * while (okay(a)) {
  //  *   act(a)
  //  *   a = next(a)
  //  * }
  //  * }}}
  //  * where the bodies of `okay`, `next`, and `act` are inlined.
  //  */
  // // Suggested by Rex Kerr here: http://www.scala-lang.org/node/9809
  // def macro cfor[A](zero: A)(okay: A => Boolean, next: A => A)(act: A => Unit) = {
  //   val util = Util(_context); import util._

  //   val elementVarName = newTermName("$a")

  //   val b = Block(
  //     List(
  //       ValDef(
  //         Modifiers(Set(reflect.api.Modifier.mutable)),
  //         elementVarName,
  //         TypeTree(),
  //         zero
  //       )
  //     ),
  //     LabelDef(
  //       newTermName("while$1"),
  //       List(),
  //       If(
  //         // okay(a)
  //         inlineApply(okay, List(Ident(elementVarName))),
  //         Block(
  //           List(
  //             Block(
  //               List(
  //                 // act(a)
  //                 inlineApply(act, List(Ident(elementVarName)))
  //               ),
  //               // a = next(a)
  //               Assign(
  //                 Ident(elementVarName),
  //                 inlineApply(next, List(Ident(elementVarName)))
  //               )
  //             )
  //           ),
  //           Apply(Ident(newTermName("while$1")), List())
  //         ),
  //         Literal(Constant(()))
  //       )
  //     )
  //   )
  //   resetAllAttrs(b)
  // }

  // //case class Lens[A, B](getter: A => B, setter: (A, B) => A)

  // /**
  //  * Automatic lens generation.
  //  *
  //  * {{{
  //  * case class Foo(a: Int)
  //  * val l = lens[Foo].a
  //  * val foo = Foo(0)
  //  * l._1(foo) // 0
  //  * l._2(foo, 1) // Foo(1)
  //  * }}}
  //  */
  // def lens[T] = new Lenser[T]

  // class Lenser[T] extends Dynamic {
  //   def macro applyDynamic(mn: String)() = {
  //     val util = Util(_context); import util._
  //     import reflect.api.Modifier
  //     println(showRaw(_this))
  //     val t = (_this, mn) match {
  //       case (TypeApply(
  //         Select(
  //           _, lensMethodTermName
  //         ), List(tpe)), Literal(Constant(methodName: String))) =>
  //         val getterMember = tpe.tpe.member(newTermName(methodName))
  //         if (getterMember == NoSymbol) sys.error("value " + methodName + " is not a member of " + tpe.tpe)
  //         val memberType = getterMember.typeSignatureIn(tpe.tpe) match {
  //           case NullaryMethodType(memberType) => memberType
  //           case _ => sys.error("member %s is not a field".format(methodName))
  //         }
  //         val getter = Function(List(ValDef(Modifiers(Set(Modifier.parameter)), newTermName("a$"), TypeTree().setType(tpe.tpe), EmptyTree)), Select(Ident(newTermName("a$")), newTermName(methodName)))
  //         val setter = Function(
  //           List(
  //             ValDef(Modifiers(Set(Modifier.parameter)), newTermName("a$"), TypeTree().setType(tpe.tpe), EmptyTree),
  //             ValDef(Modifiers(Set(Modifier.parameter)), newTermName("x$"), TypeTree().setType(memberType), EmptyTree)
  //           ),
  //           Apply(Select(Ident(newTermName("a$")), newTermName("copy")), List(AssignOrNamedArg(Ident(newTermName(methodName)), Ident(newTermName("x$")))))
  //         )
  //         val getterSetter = Apply(Select(Select(Ident(newTermName("scala")), newTermName("Tuple2")), newTermName("apply")), List(getter, setter))
  //         getterSetter
  //       case x => sys.error("unexpected _this tree: " + x)
  //     }
  //     resetAllAttrs(t)
  //   }
  // }

  // import scala.reflect.macro.Context

  // implicit def Util(context: Context) = new Util[context.type](context)

  // class Util[C <: Context with Singleton](val context: C) {
  //   import context._

  //   def id(a: Tree): Tree = a

  //   // The first version of the trace macro did not call this, which led to NSDNHO
  //   // errors with `trace(1.toString.toString)`.
  //   //
  //   // Explanation from Eugene:
  //   //
  //   // The trouble was with the fact that the tree produced by trace(1.toString.toString)
  //   // was partially typed. And when some AST already has a type, typer doesn't drill into
  //   // its children and just moves along. Consequently, newly generated valdefs were never
  //   // processed by the typer, i.e. never got symbols assigned to them, hence the NSDNHO.
  //   def resetAllAttrs(a: Tree): Tree = {
  //     val global = context.asInstanceOf[scala.tools.nsc.Global]
  //     global.resetAllAttrs(a.asInstanceOf[global.Tree])
  //            .asInstanceOf[Tree]
  //   }

  //   def literalFunctionToLocalMethod(f: Tree, methodName: TermName): Tree = {
  //     f match {
  //         case Function(params, body)  =>
  //           DefDef(Modifiers(), methodName, List(), List(params), TypeTree(), body)
  //         case _ =>  sys.error("parameter `f` must be a function literal.")
  //     }
  //   }

  //   /**
  //    * In:
  //    * `((p1, p2, ... pN) => <body>).apply(a1, a2, ..., aN)`
  //    *
  //    * Out:
  //    * ```
  //    * val p1 = a1; val p2 = a2; ... val pN = aN;
  //    * <body>
  //    * ````
  //    */
  //   def inlineApply(f: Tree, args: List[Tree]): Tree = {
  //     f match {
  //         case Function(params, body)  =>
  //           if (params.length != args.length) sys.error("incorrect arity")
  //           // val a = args(0); val b = args(1); ...
  //           val paramVals = params.zip(args).map {
  //             case (ValDef(_, pName, _, _), a) =>
  //               ValDef(Modifiers(), pName, TypeTree(), a)
  //           }
  //           Block(paramVals, body)
  //         case _ =>  sys.error("parameter `f` must be a function literal.")
  //     }
  //   }

  //   def predefAssert: Tree =
  //     predefSelect("assert")

  //   def predefPrintln: Tree =
  //     predefSelect("println")

  //   def predefPrint: Tree =
  //     predefSelect("print")

  //   def predefSelect(name: String): Tree =
  //     Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName(name))

  //   def stringLit(s: String): Tree =
  //     Literal(Constant(s))

  //   def withNumericAndLhs(_this: Tree)(f: (Tree, Tree) => Tree) = {
  //     _this match {
  //       case Apply(Apply(TypeApply(_ /*infixNumericOps*/, _), List(lhs)), List(numeric)) =>
  //         f(numeric, lhs)
  //       case t => sys.error("unexpected tree: " + show(t))
  //     }
  //   }

  //   def unaryNumericOp(_this: Tree, methodName: String) = withNumericAndLhs(_this) {
  //     (numeric, lhs) =>
  //       Apply(Select(numeric, newTermName(methodName)), List(lhs))
  //   }

  //   def binaryNumericOp(_this: Tree, methodName: String, rhs: Tree) = withNumericAndLhs(_this) {
  //     (numeric, lhs) =>
  //       Apply(Select(numeric, newTermName(methodName)), List(lhs, rhs))
  //   }
  // }
}