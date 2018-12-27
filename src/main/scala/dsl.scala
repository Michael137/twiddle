package twiddle.dsl

import scala.lms.common._
import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import scala.collection.BitSet
import scala.language.higherKinds
import scala.math.pow
import scala.language.implicitConversions

object Syntax {
  trait Arithmetic[T[_]] {
    // implicit def int2Float(n: Int) = n.toFloat
    // implicit def float2Int(f: Float) = f.toInt
    // implicit def double2Int(d: Double) = d.toInt

    def num[A](v: A): T[A]
    def add[A: Numeric](a: T[A], b: T[A]): T[A]
    def sub[A: Numeric](a: T[A], b: T[A]): T[A]
    def mul[A: Numeric](a: T[A], b: T[A]): T[A]
    def div(a: T[Int], b: T[Int]): T[Int]
    def mod(a: T[Int], b: T[Int]): T[Int] // Modulus
  }

  trait Bools[T[_]] {
    def bool(b: Boolean): T[Boolean]
    def if_[A] : T[Boolean] => (() => T[A]) => (() => T[A]) => T[A]
  }

  trait Lambda[T[_]] {
    def lam[A,B](f: (T[A] => T[B])): T[A => B]
    def app[A, B] : T[A => B] => (T[A] => T[B])
  }

  trait CMathOps[T[_]] {
    def num[A](v: A): T[A]
    def log2(a: T[Int]): T[Int]
  }
  
  trait Exp[T[_]] extends Bools[T] with Arithmetic[T] with Lambda[T]
  trait CExp[T[_]] extends CMathOps[T]
}

object CodeGen {
  import Syntax._

  type AST[A] = List[Term]
  abstract class Term
  case class IntPtr() extends Term
  case class Const(e: Term) extends Term
  case class Addr(e: Term) extends Term
  case class Var(s: String) extends Term
  case class Num[A](n: A) extends Term
  case class Assign(v: Var, e: Term) extends Term
  case class Decl(v: Term) extends Term
  case class I(v: Var) extends Term // Integer
  case class F(v: Var) extends Term // Float
  case class D(v: Var) extends Term // Double
  case class Rshift[A](v: Term, n: Num[A]) extends Term // >>
  case class Minus[A](v: Term, n: Num[A]) extends Term // -
  case class Cast(c: Term, e: Term) extends Term // -
  
  implicit object EmitTwiddleAST extends CExp[AST] {
    def num[A](v: A) = List(Num(v))
    def log2(a: AST[Int]): AST[Int] = {
      val List(t) = a
      List(Decl(F(Var("x0"))),
           Decl(I(Var("x1"))),
           Assign(Var("x0"), t),
           Assign(Var("x1"), Cast(Const(IntPtr()), Addr(Var("x0")))),
           Assign(Var("x1"), Minus(Rshift(Var("x1"), Num(23)), Num(127))))
    }
  }

  // TODO: Tagless interpreter for Twiddle AST
  // either eval/pretty print code or generete C code
}

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
    def if_[A] : Boolean => (() => A) => (() => A) => A = b => t => e => if (b) { t () } else { e () }
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
    def if_[A] : String => (() => String) => (() => String) => String =
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

object Examples {
  import Syntax._
  import CodeGen._
  import Interpreter._
  def example1[T[_]](s:Exp[T]) : T[Double] = {
    import s._
    // the term is
    //    if true then 3 + 4
    //    else 5
    (if_(bool(true))
      (() => add(num(3.5), num(3.6)))
      (() => num(5.5)))
  }

  def example2[T[_]](s:Exp[T]) : T[(Int => Int) => (Int => Int)] = {
    import s._
    lam[Int => Int,Int => Int] (f =>
      lam[Int,Int] (x => 
        app (f) (app(f)(x))))
  }

  def example3[T[_]](s:CExp[T]) : T[Int] = {
    import s._
    log2(num(20))
  }
}