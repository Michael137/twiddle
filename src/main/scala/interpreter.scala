package twiddle.dsl

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import collection.immutable._ // For BitSet
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.math._
import shapeless._
import syntax.typeable._

object Interpreter {
  import Syntax._

  // Simple evaluator
  type Id[A] = A
  implicit object Eval extends Exp[Id] {
    implicit def d2i(x: Double): Int = x.toInt
    def bits(a: Int): BitSet = BitSet.fromBitMaskNoCopy(Array(a))
    def reverseBitsParallel(b: BitSet): BitSet = reverseBits(b)
    def reverseBits(b: BitSet): BitSet = {
      var bitlen = 32 // b.reduceLeft(_ max _)
      val arr = b.toArray
      var flipped = (1 to bitlen).diff(arr).toArray
      collection.immutable.BitSet.empty ++ flipped
    }
    def hasZero(b: BitSet): Boolean = (b.toArray).size != 32
    def swapBits(a: BitSet, b: BitSet): (BitSet, BitSet) = (b, a)
    
    def add[A: Numeric](a: A, b: A): A = implicitly[Numeric[A]].plus(a,b)
    def sub[A: Numeric](a: A, b: A): A = implicitly[Numeric[A]].minus(a,b)
    def mul[A: Numeric](a: A, b: A): A = implicitly[Numeric[A]].times(a,b)
    def div(a: Int, b: Int): Int = a / b
    def mod(a: Int, b: Int): Int = a % b
    def num[A](v: A) = v
    def bool(v: Boolean) = v
    def ifThenElse[A](b: Id[Boolean])(t: (() => Id[A]))(e: (() => Id[A]))(implicit tag: ClassTag[A]): Id[A] = if (b) { t () } else { e () }
    def ifThen[A](b: Id[Boolean])(t: (() => Id[A]))(implicit tag: ClassTag[A]): Id[Unit] = if (b) { t () }
    def lam[A: ClassTag, B: ClassTag](f : A => B) = f
    def app[A, B] = (f : A => B) => (p : A) => f(p)
    def cons(a: Any, b: Any) = (a, b)
    def car(t: Any): Any = {
      val (hd, _) = t
      hd
    }
    def cdr(t: Any): Any = {
      val (_, tl) = t
      tl
    }
    def log10(a: Double): Double = (scala.math.log10(a))
    def log2(a: Double): Double = (scala.math.log10(a)/scala.math.log10(2))
    def ternaryIf[A] : Boolean => (() => A) => (() => A) => A = b => t => e => if (b) { t () } else { e () }
    def string(s: String): String = s
    def reverse(a: String): String = a.reverse
    def begin[A](as: List[A]): A = { println(as);as(0) }
    def prints[A](format: String, es: List[A]): Unit = print(es)
    def and(a: Boolean, b: Boolean): Boolean = a && b
    def or(a: Boolean, b: Boolean): Boolean = a || b
  }

  // Pretty-printer
  type CString[A] = String
  implicit object Show extends Exp[CString] {
    implicit def d2i(x: String): String = x
    def bits(a: String): String = {
      var s = ""
      val bits = BitSet.fromBitMaskNoCopy(Array(a.toLong))
      val bitlen = 32 // bits.reduceLeft(_ max _)
      for(i <- 1 to bitlen)
        if(bits(i)) s += "1" else s += "0"
      "0b" + s.reverse
    }
    def reverseBitsParallel(b: String): String = reverseBits(b)
    def reverseBits(b: String): String = {
      var s = b.substring(2)
      "0b" + ((s.replaceAll("0", "#")).replaceAll("1", "0")).replaceAll("#", "1")
    }
    def hasZero(b: String): String = s"(hasZero? $b)"
    def swapBits(a: String, b: String): String = s"($b, $a)"

    def add[A: Numeric](a: CString[A], b: CString[A]): CString[A] = "("+ a + " + " + b + ")"
    def sub[A: Numeric](a: CString[A], b: CString[A]): CString[A] = "("+ a + " - " + b + ")"
    def mul[A: Numeric](a: CString[A], b: CString[A]): CString[A] = "("+ a + " * " + b + ")"
    def div(a: String, b: String): String = "("+ a + " / " + b + ")"
    def mod(a: String, b: String): String = "("+ a + " % " + b + ")"
    def num[A](v: A): String = v.toString
    def bool(v: Boolean): String = v.toString
    def ifThenElse[A](b: CString[Boolean])(t: (() => String))(e: (() => String))(implicit tag: ClassTag[A]): String =
            "(if "+ b +" then "+ t() +" else "+ e() +")"
    def ifThen[A](b: CString[Boolean])(t: (() => String))(implicit tag: ClassTag[A]): String =
                "(if "+ b +" then "+ t() + ")"
    var counter = 1
    def lam[A: ClassTag, B: ClassTag](f : String => String) = {
      val x = s"x$counter"
      counter += 1
      val body = f(x)
      s"\\$x.$body"
    }
    def app[A,B] = (f : String) => (p : String) => s"($f $p)"
    def cons(a: Any, b: Any): CString[Any] = s"(cons $a $b)"
    def car(t: Any): Any = s"(car $t)"
    def cdr(t: Any): Any = s"(cdr $t)"
    def log10(a: String): String = s"log10($a)"
    def log2(a: String): String = s"log10($a)/log10(2)"
    
    def ternaryIf[A] : String => (() => String) => (() => String) => String =
            b => t => e =>
            "("+ b +") ? "+ t() +" : "+ e()

    def string(s: String): String = s
    def reverse(a: String): String = s""""$a".reverse"""

    def begin[A](as: List[String]): String = {
      var ret = "(begin "
      as.map({s => ret += s", $s"})
      ret + ")"
    }

    def prints[A](format: String, es: List[String]): String = {
      var ret = "(print "
      es.map({s => ret += s", $s"})
      ret + ")"
    }
    def and(a: String, b: String): String = s"$a && $b"
    def or(a: String, b: String): String = s"$a || $b"
  }
}