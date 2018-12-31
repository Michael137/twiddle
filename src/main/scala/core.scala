package twiddle.dsl

import scala.lms.common._
import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import scala.collection.BitSet
import scala.language.higherKinds
import scala.math.pow
import scala.language.implicitConversions

object Syntax {
  trait Nums[T[_]] {
    def num[A](v: A): T[A]
  }

  trait Arithmetic[T[_]] {
    def add[A: Numeric](a: T[A], b: T[A]): T[A]
    def sub[A: Numeric](a: T[A], b: T[A]): T[A]
    def mul[A: Numeric](a: T[A], b: T[A]): T[A]
    def div(a: T[Int], b: T[Int]): T[Int]
    def mod(a: T[Int], b: T[Int]): T[Int] // Modulus
  }

  trait Bools[T[_]] {
    def bool(b: Boolean): T[Boolean]
    def ifThenElse_[A] : T[Boolean] => (() => T[A]) => (() => T[A]) => T[A]
  }

  trait Lambda[T[_]] {
    def lam[A,B](f: (T[A] => T[B])): T[A => B]
    def app[A, B] : T[A => B] => (T[A] => T[B])
  }

  trait LispLike[T[_]] {
    // def cons : T[Any] => T[Any] => T[Tuple2[T[Any], T[Any]]]
    def cons(a: Any, b: Any): T[(T[Any], T[Any])] // ? narrow type definitions
    def car(t: T[(T[Any], T[Any])]): T[(T[Any], T[Any])]
    def cdr(t: T[(T[Any], T[Any])]): T[(T[Any], T[Any])]
  }

  trait CLike[T[_]] {
    def ternaryIf[A] : T[Boolean] => (() => T[A]) => (() => T[A]) => T[A]
  }

  trait CMathOps[T[_]] {
    def log2(a: T[Int]): T[Int]
    def log10(a: T[Int]): T[Int]
  }
  
  trait Exp[T[_]] extends CMathOps[T] with Bools[T] with Nums[T] with Arithmetic[T] with LispLike[T] with CLike[T] with Lambda[T]
  trait CExp[T[_]] extends CMathOps[T] with Bools[T] with Nums[T] with Arithmetic[T] with LispLike[T] with CLike[T] // TODO: simply extend Exp; with Arithmetic[T] with
}