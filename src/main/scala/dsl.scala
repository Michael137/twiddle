package twiddle.dsl

import scala.lms.common._
import scala.reflect._
import scala.collection.BitSet

trait TwDsl {
  // Abstract type
  type E

  def int(i: Int): E
  def bits(b: BitSet): E

  def infix_+%(a: E, b: E): E
}

object Twiddle {
  // From: https://github.com/scala-lms/tutorials/blob/master/src/test/scala/lms/tutorial/dslapi.scala
//  trait CCodeGen extends CGenNumericOps with CGenPrimitiveOps with CGenBooleanOps {
//
//  }

  trait TwInterp extends TwDsl {
    abstract class Value
    case class I(v: Int) extends Value
    case class B(v: BitSet) extends Value

    override type E = Value
    override def int(i: Int): E = I(i)
    override def infix_+%(a: E, b: E) : E = 
  }
}
