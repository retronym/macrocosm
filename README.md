An exploration of [Scala macros](http://scala-macros.org).

Here's what we've got so far:

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

scala> log("".isEmpty)
"".isEmpty() = true
res1: Boolean = true

scala> assert1("foo".reverse == "off")
java.lang.AssertionError: assertion failed: scala.this.Predef.augmentString("foo").reverse.==("off")
	at scala.Predef$.assert(Predef.scala:161)
	at .<init>(<console>:10)


scala> assert2("foo".reverse == "off")
java.lang.AssertionError: assertion failed: assert2("foo".reverse == "off")
	at scala.Predef$.assert(Predef.scala:161)
	at .<init>(<console>:10)

scala> def plus(a: Int, b: Int) = a + b
plus: (a: Int, b: Int)Int

scala> trace(plus(1, plus(2, plus(3, 4))))
$line1.$read.$iw.$iw.plus(3, 4) = 7
$line1.$read.$iw.$iw.plus(2, $line1.$read.$iw.$iw.plus(3, 4)) = 9
$line1.$read.$iw.$iw.plus(1, $line1.$read.$iw.$iw.plus(2, $line1.$read.$iw.$iw.plus(3, 4))) = 10
res3: Int = 10

scala> b"102"
<console>:11: error: exception during macro expansion: invalid binary literal
              b"102"
              ^
scala> b"101010"
res4: Int = 42

```
