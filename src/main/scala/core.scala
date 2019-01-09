package twiddle.dsl

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import collection.immutable._ // For BitSet
import scala.language.higherKinds
import scala.math.pow
import scala.language.implicitConversions
import scala.annotation.varargs
import spire.syntax._

object Syntax {
  trait Nums[T[_]] {
    def num[A](v: A): T[A]
  }

  trait NullType[T[_]] {
    def null_(): T[Any]
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
    def ifThen[A](b: T[Boolean])(t: (() => T[A]))(implicit tag: ClassTag[A]): T[Any]
    def ternaryIf[A] : T[Boolean] => (() => T[A]) => (() => T[A]) => T[A]
    def and(a: T[Boolean], b: T[Boolean]): T[Boolean]
    def or(a: T[Boolean], b: T[Boolean]): T[Boolean]
    def lt[A <% Ordered[A]](a: T[A], b: T[A]): T[Boolean]
  }

  trait Lambda[T[_]] {
    def lam[A: ClassTag, B: ClassTag](f: (T[A] => T[B])): T[A => B]
    def app[A, B] : T[A => B] => (T[A] => T[B])
  }

  trait Loops[T[_]] {
    // Codegen should transform
    // from: for(i <- 0 until 10) { /* do something */ }
    // to: for(i = 0; i < 10; i = i + 1) { /* do something */ }
    def for_(init: T[Int], cond: T[Int] => T[Boolean], variant: T[Int] => T[Int], body: T[Int] => T[Unit]): T[Unit]
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
    def prints[A: ClassTag](format: String, es: T[A]): T[Unit]
  }

  trait Bits[T[_]] {
    implicit def d2i(x: T[Double]): T[Int]
    def bits(a: T[Int]): T[BitSet]
    def reverseBits(b: T[BitSet]): T[BitSet]
    def reverseBitsParallel(b: T[BitSet]): T[BitSet]
    def hasZero(b: T[BitSet]): T[Boolean]
    def swapBits(a: T[BitSet], b: T[BitSet]): T[(BitSet, BitSet)]

    // bitParity
  }

  trait Test[T[_]] {}
  
  trait Exp[T[_]] extends CMathOps[T] with Bools[T] with Nums[T] with Arithmetic[T] with LispLike[T]
                                      with CStrOps[T] with Strings[T] with IOOps[T] with Lambda[T]
                                      with Bits[T] with Loops[T] with NullType[T]
  
  // Eventually not needed once EmitParallelAST supports all language features
  trait ParallelExp[T[_]] extends Loops[T] with Bools[T] with Nums[T] with Arithmetic[T]
                                           with IOOps[T] with NullType[T]
}