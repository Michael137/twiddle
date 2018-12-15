package twiddle.dsl

import scala.lms.common._
import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import scala.collection.BitSet
import scala.language.higherKinds
import scala.math.pow

object Syntax {
  trait Arithmetic[T[_]] {
    def int(v: Int): T[Int]
    def add(a: T[Int], b: T[Int]): T[Int]
    def sub(a: T[Int], b: T[Int]): T[Int]
    def mul(a: T[Int], b: T[Int]): T[Int]
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
  
  trait Exp[T[_]] extends Bools[T] with Arithmetic[T] with Lambda[T]
}

object Interpreter {
  import Syntax._

  // Simple evaluator
  type Id[A] = A
  implicit object Eval extends Exp[Id] {
    def add(a: Int, b: Int): Int = a + b
    def sub(a: Int, b: Int): Int = a - b
    def mul(a: Int, b: Int): Int = a * b
    def div(a: Int, b: Int): Int = a / b
    def mod(a: Int, b: Int): Int = a % b
    def int(v: Int) = v
    def bool(v: Boolean) = v
    def if_[A] : Boolean => (() => A) => (() => A) => A = b => t => e => if (b) { t () } else { e () }
    def lam[A, B](f : A => B) = f
    def app[A, B] = (f : A => B) => (p : A) => f(p)
  }

  // Pretty-printer
  type CString[A] = String
  implicit object Show extends Exp[CString] {
    def add(a: String, b: String): String = "("+ a + " + " + b + ")"
    def sub(a: String, b: String): String = "("+ a + " - " + b + ")"
    def mul(a: String, b: String): String = "("+ a + " * " + b + ")"
    def div(a: String, b: String): String = "("+ a + " / " + b + ")"
    def mod(a: String, b: String): String = "("+ a + " % " + b + ")"
    def int(v: Int): String = v.toString
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

  def example1[T[_]](s:Exp[T]) : T[Int] = {
    import s._
    // the term is
    //    if true then 3 + 4
    //    else 5
    (if_(bool(true))
      (() => add(int(3), int(4)))
      (() => int(5)))
  }

  def example2[T[_]](s:Exp[T]) : T[(Int => Int) => (Int => Int)] = {
    import s._
    lam[Int => Int,Int => Int] (f =>
      lam[Int,Int] (x => 
        app (f) (app(f)(x))))
  }
}