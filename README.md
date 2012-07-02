An exploration of [Scala macros](http://scalamacros.org/).

This branch has been adapted for the new syntax proposed in [SIP-16](http://docs.scala-lang.org/sips/pending/self-cleaning-macros.html)

## Prerequisites

### SBT 0.12.0-RC3

Use the latest [launcher](http://repo.typesafe.com/typesafe/ivy-snapshots/org.scala-sbt/launcher/). You probably want to start SBT without using your global `~/.sbt` folder with incompatible plugins.

The easiest way to obtain the right SBT version, and to use a separate .sbt directory is to use Paul Phillips' [sbt-extras launcher script](https://github.com/paulp/sbt-extras/blob/master/sbt).

## Now, the macros!

### Compiler Diagnostics / Tree Manipulation

```
> console
[info] Compiling 1 Scala source to /Users/jason/code/scala-spock/target/scala-2.10.0-SNAPSHOT/classes...
[info] Starting scala interpreter...
[info]
Welcome to Scala version 2.10.0-M1-0362-g2fe570291a-2012-02-18 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_29).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import com.github.retronym.macrocosm.Macrocosm._
import com.github.retronym.macrocosm.Macrocosm._

scala> desugar("foo".map(_ + 1))
res0: String = scala.this.Predef.augmentString("foo").map[Int, Any](((x$1: Char) => x$1.+(1)))(scala.this.Predef.fallbackStringCanBuildFrom[Int])
```

The following work without macrocosm.

```
scala> val t = reflect.mirror.reify("abc".reverse)
t: reflect.mirror.Expr[String] = Expr[null](scala.this.Predef.augmentString("abc").reverse)

scala> reflect.mirror.showRaw(t.tree)
res3: String = Select(Apply(Select(Select(This(newTypeName("scala")), newTermName("Predef")), newTermName("augmentString")), List(Literal(Constant("abc")))), newTermName("reverse"))

scala> val tb = new reflect.runtime.Mirror.ToolBox()
tb: reflect.runtime.Mirror.ToolBox = scala.reflect.runtime.ToolBoxes$ToolBox@55b6ed96

scala> tb.typeCheck(t.tree)
res5: reflect.mirror.Tree = scala.this.Predef.augmentString("abc").reverse

scala> .tpe
res6: reflect.mirror.Type = String

scala> tb.runExpr(t.tree)
res8: Any = cba
```

Especially fun is using `desugar` to see what the other macros here have done!

```
scala> desugar{def foo[T: Numeric](t: T) = t - t}
res7: String = 
{
  def foo[T >: Nothing <: Any](t: T)(implicit evidence$1: Numeric[T]): T = evidence$1.minus(t, t);
  ()
}

scala> desugar{var i = 0; cfor(0)(_ < 10, _ + 1)((a: Int) => i += 1)}
res8: String = 
{
  var i: Int = 0;
  {
    var $a: Int = 0;
    while$1(){
      if ({
        val x$1: Int = $a;
        x$1.<(10)
      })
        {
          {
            {
              val a: Int = $a;
              i = i.+(1)
            };
            $a = {
              val x$2: Int = $a;
              x$2.+(1)
            }
          };
          while$1()
        }
      else
        ()
    }
  }
}
```

### Logging / Assertions
```
scala> log("".isEmpty)
"".isEmpty() = true
res1: Boolean = true

scala> assert1("foo".reverse == "off")
java.lang.AssertionError: assertion failed: scala.this.Predef.augmentString("foo").reverse.==("off")
	at scala.Predef$.assert(Predef.scala:161)
	at .<init>(<console>:10)

scala> def plus(a: Int, b: Int) = a + b
plus: (a: Int, b: Int)Int

scala> trace(plus(1, plus(2, plus(3, 4))))
$line1.$read.$iw.$iw.plus(3, 4) = 7
$line1.$read.$iw.$iw.plus(2, $line1.$read.$iw.$iw.plus(3, 4)) = 9
$line1.$read.$iw.$iw.plus(1, $line1.$read.$iw.$iw.plus(2, $line1.$read.$iw.$iw.plus(3, 4))) = 10
res3: Int = 10
```

### New literals

```
scala> b"101010"
res4: Int = 42

scala> b"102"
<console>:11: error: exception during macro expansion: invalid binary literal
              b"102"
              ^
```

### Statically Checked Regex

```
scala> regex(".*")
res0: scala.util.matching.Regex = .*

scala> regex("{")
<console>:11: error: exception during macro expansion: Illegal repetition
{
              regex("{")
                   ^

```

### High Performance Loops

Translated to `while` loops, eliminate the indirection through `FunctionN`, and avoids boxing.

```
scala> cfor(0)(_ < 10, _ + 2)(println(_))
0
2
4
6
8

scala> val as = Array(1, 2, 3)
as: Array[Int] = Array(1, 2, 3)

scala> iteratorForeach(Iterator(1, 2, 3, 4, 5))(println(_))
1
2
3
4
5

```

### Implicit Wrapper Elimination

Rewrite `enrich[T](lhs)(numericInstance).*(rhs)` to `numericInstance.plus(lhs, rhs)` 
by defining `*` as a macro def.

```
scala> import com.github.retronym.macrocosm.Macrocosm._
import com.github.retronym.macrocosm.Macrocosm._

scala> object A { def foo[T: Numeric](t: T) = (-t * t).abs }
 public java.lang.Object foo(java.lang.Object, scala.math.Numeric);
  Code:
   Stack=4, Locals=3, Args_size=3
   0:   aload_2
   1:   aload_2
   2:   aload_2
   3:   aload_1
   4:   invokeinterface #21,  2; //InterfaceMethod scala/math/Numeric.negate:(Ljava/lang/Object;)Ljava/lang/Object;
   9:   aload_1
   10:  invokeinterface #25,  3; //InterfaceMethod scala/math/Numeric.times:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;
   15:  invokeinterface #28,  2; //InterfaceMethod scala/math/Numeric.abs:(Ljava/lang/Object;)Ljava/lang/Object;
   20:  areturn
  LineNumberTable:
   line 10: 2
```

### Dynamic Lens Creation

This even works for case classes defined in the current compile run.

```
scala> case class Person(name: String, age: Int)
defined class Person

scala> val nameLens = lens[Person].name
dynatype: com.github.retronym.macrocosm.Macrocosm.lens[Person].applyDynamic("name")
TypeApply(Select(Select(Select(Select(Select(Ident(newTermName("com")), newTermName("github")), newTermName("retronym")), newTermName("macrocosm")), newTermName("Macrocosm")), newTermName("lens")), List(TypeTree().setType(Person)))
nameLens: (Person => String, (Person, String) => Person) = (<function1>,<function2>)

scala> val p = Person("brett", 21)
p: Person = Person(brett,21)

scala> val nameLens = lens[Person].name
dynatype: com.github.retronym.macrocosm.Macrocosm.lens[Person].applyDynamic("name")()
TypeApply(Select(Select(Select(Select(Select(Ident(newTermName("com")), newTermName("github")), newTermName("retronym")), newTermName("macrocosm")), newTermName("Macrocosm")), newTermName("lens")), List(TypeTree().setType(Person)))
nameLens: (Person => String, (Person, String) => Person) = (<function1>,<function2>)

scala> nameLens._1(p)
res1: String = brett

scala> nameLens._2(p, "bill")
res2: Person = Person(bill,21)

scala> lens[Person].namexx(())
dynatype: com.github.retronym.macrocosm.Macrocosm.lens[Person].applyDynamic("namexx")()
TypeApply(Select(Select(Select(Select(Select(Ident(newTermName("com")), newTermName("github")), newTermName("retronym")), newTermName("macrocosm")), newTermName("Macrocosm")), newTermName("lens")), List(TypeTree().setType(Person)))
error: exception during macro expansion: value namexx is not a member of Person
```