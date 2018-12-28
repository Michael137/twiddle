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
    def ifThenElse_[A] : T[Boolean] => (() => T[A]) => (() => T[A]) => T[A]
  }

  trait Lambda[T[_]] {
    def lam[A,B](f: (T[A] => T[B])): T[A => B]
    def app[A, B] : T[A => B] => (T[A] => T[B])
  }

  trait CMathOps[T[_]] {
    def log2(a: T[Int]): T[Int]
  }
  
  trait Exp[T[_]] extends Bools[T] with Arithmetic[T] with Lambda[T]
  trait CExp[T[_]] extends Bools[T] with Arithmetic[T] with CMathOps[T] // TODO: simply extend Exp
}