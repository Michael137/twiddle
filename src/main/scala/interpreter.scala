package twiddle.dsl

import scala.lms.common._
import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import scala.collection.BitSet
import scala.language.higherKinds
import scala.math.pow
import scala.language.implicitConversions

object Interpreter {
  import Syntax._

  // Simple evaluator
  type Id[A] = A
  implicit object Eval extends Exp[Id] {
    def add[A: Numeric](a: A, b: A): A = implicitly[Numeric[A]].plus(a,b)
    def sub[A: Numeric](a: A, b: A): A = implicitly[Numeric[A]].minus(a,b)
    def mul[A: Numeric](a: A, b: A): A = implicitly[Numeric[A]].times(a,b)
    def div(a: Int, b: Int): Int = a / b
    def mod(a: Int, b: Int): Int = a % b
    def num[A](v: A) = v
    def bool(v: Boolean) = v
    def ifThenElse_[A] : Boolean => (() => A) => (() => A) => A = b => t => e => if (b) { t () } else { e () }
    def lam[A, B](f : A => B) = f
    def app[A, B] = (f : A => B) => (p : A) => f(p)
  }

  // Pretty-printer
  type CString[A] = String
  implicit object Show extends Exp[CString] {
    def add[A: Numeric](a: CString[A], b: CString[A]): CString[A] = "("+ a + " + " + b + ")"
    def sub[A: Numeric](a: CString[A], b: CString[A]): CString[A] = "("+ a + " - " + b + ")"
    def mul[A: Numeric](a: CString[A], b: CString[A]): CString[A] = "("+ a + " * " + b + ")"
    def div(a: String, b: String): String = "("+ a + " / " + b + ")"
    def mod(a: String, b: String): String = "("+ a + " % " + b + ")"
    def num[A](v: A): String = v.toString
    def bool(v: Boolean): String = v.toString
    def ifThenElse_[A] : String => (() => String) => (() => String) => String =
            b => t => e =>
            "(if "+ b +" then "+ t() +" else "+ e() +")"
    var counter = 1
    def lam[A, B](f : String => String) = {
      val x = s"x$counter"
      counter += 1
      val body = f(x)
      s"\\$x.$body"
    }
    def app[A,B] = (f : String) => (p : String) => s"($f $p)"
  }
}