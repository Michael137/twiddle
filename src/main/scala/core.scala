package twiddle.dsl

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import collection.immutable._ // For BitSet
import scala.language.higherKinds
import scala.math.pow
import scala.language.implicitConversions
import scala.annotation.varargs

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
    def ifThenElse[A](b: T[Boolean])(t: (() => T[A]))(e: (() => T[A]))(implicit tag: ClassTag[A]): T[A]
    def ifThen[A](b: T[Boolean])(t: (() => T[A]))(implicit tag: ClassTag[A]): T[Unit]
    def ternaryIf[A] : T[Boolean] => (() => T[A]) => (() => T[A]) => T[A]
    def and(a: T[Boolean], b: T[Boolean]): T[Boolean]
    def or(a: T[Boolean], b: T[Boolean]): T[Boolean]
  }

  trait Lambda[T[_]] {
    def lam[A: ClassTag, B: ClassTag](f: (T[A] => T[B])): T[A => B]
    def app[A, B] : T[A => B] => (T[A] => T[B])
  }

  trait LispLike[T[_]] {
    def cons(a: Any, b: Any): T[(T[Any], T[Any])] // ? narrow type definitions
    def begin[A](as: List[T[A]]): T[A]
    def car(t: Any): Any
    def cdr(t: Any): Any
  }

  trait CMathOps[T[_]] {
    def log2(a: T[Double]): T[Double]
    def log10(a: T[Double]): T[Double]
  }

  trait Strings[T[_]] {
    def string(s: String): T[String]
  }

  trait CStrOps[T[_]] {
    def reverse(a: T[String]): T[String]
  }

  trait IOOps[T[_]] {
    def prints[A](format: String, es: List[T[A]]): T[Unit]
  }

  trait Bits[T[_]] {
    implicit def d2i(x: T[Double]): T[Int]
    def bits(a: T[Int]): T[BitSet]
    def reverseBits(b: T[BitSet]): T[BitSet]
    def reverseBitsParallel(b: T[BitSet]): T[BitSet]
    def hasZero(b: T[BitSet]): T[Boolean]
    def swapBits(a: T[BitSet], b: T[BitSet]): T[(BitSet, BitSet)]
  }

  trait Parallel[T[_]] {}

  trait Test[T[_]] {}
  
  trait Exp[T[_]] extends CMathOps[T] with Bools[T] with Nums[T] with Arithmetic[T] with LispLike[T]
                                      with CStrOps[T] with Strings[T] with IOOps[T] with Lambda[T]
                                      with Bits[T]
}