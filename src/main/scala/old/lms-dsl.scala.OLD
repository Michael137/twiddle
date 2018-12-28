// Source: https://github.com/scala-lms/tutorials/blob/master/src/test/scala/lms/tutorial/dslapi.scala

package twiddle.lms.dsl

import scala.lms.common._
import scala.lms.internal._
import scala.reflect.SourceContext
import scala.io._
import java.io.{PrintStream, OutputStream}

/*
  TODO: Reverse string implementation (from https://codereview.stackexchange.com/questions/58959/is-this-a-fast-implementation-of-reversing-a-string):
  char * StringReverse(char * str) 
{
    assert(str != NULL);
    if (!*str) 
    {
        // Empty string, do nothing
        return (str);
    }

    char * p1;
    char * p2;
    for (p1 = str, p2 = str + strlen(str) - 1; p2 > p1; ++p1, --p2) 
    {
        // XOR trick to swap integer values:
        *p1 ^= *p2;
        *p2 ^= *p1;
        *p1 ^= *p2;
    }

    return (str);
}
*/

trait MathOps extends Base { this: Dsl =>
  def tw_log10(a: Rep[Int]): Rep[Int]
  def tw_log2(a: Rep[Float]): Rep[Int]
}
trait MathOpsExp extends MathOps with BaseExp { this: DslExp =>
  case class Log10(a: Rep[Int]) extends Def[Int]
  def tw_log10(a: Exp[Int]): Exp[Int] = Log10(a)
  
  case class Log2(a: Rep[Float]) extends Def[Int]
  def tw_log2(a: Exp[Float]): Exp[Int] = Log2(a)
}
trait CGenMathOps extends CGenBase {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Log10(a) =>
      val qa = quote(a)
      emitValDef(sym, s"""($qa >= 1000000000) ? 9 : ($qa >= 100000000) ? 8 : ($qa >= 10000000) ? 7 : 
                          ($qa >= 1000000) ? 6 : ($qa >= 100000) ? 5 : ($qa >= 10000) ? 4 : 
                          ($qa >= 1000) ? 3 : ($qa >= 100) ? 2 : ($qa >= 10) ? 1 : 0;""")
    case Log2(a) =>
      val qa = quote(a)
      emitValDef(sym, s"""0""")
      emitAssignment(sym, s"""*(const int *) &${qa};""")
      emitAssignment(sym, s"""(${quote(sym)} >> 23) - 127;""")
    case _ => super.emitNode(sym, rhs)
  }
}

trait StrOps extends Base { this: Dsl =>
  def tw_reverse(a: Rep[String]): Rep[String]
}
trait StrOpsExp extends StrOps with BaseExp { this: DslExp =>
  case class ReverseString(a: Rep[String]) extends Def[String]
  def tw_reverse(a: Exp[String]): Exp[String] = ReverseString(a)
}
trait CGenStrOps extends CGenBase {
  val IR: StrOpsExp
  import IR._

  // TODO: char* instead of string
  // TODO: remove .c_str()
  // TODO: create fresh variables from emitNode
  // override def remap[A](m: Typ[A]): String = {
  //   val s = m.toString
  //   if (s.startsWith("Array["))
  //     return remapWithRef(m.typeArguments.head)
  //   val tpe = super.remap(m)
  //   if (tpe.startsWith("int") || tpe.startsWith("uint") || tpe=="bool")
  //     if (s == "Char") "char" else "int"
  //     else if (tpe == "string") "char "
  //     else tpe
  // }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReverseString(a) =>
      emitVarDecl(sym)   
    case _ => super.emitNode(sym, rhs)
  }
}

// DSL Interface
/*
  LiftVariables: to stage variables i.e. be able to pass variables to staged code
  LiftNumeric: to allow mixed non-Rep and Rep integers in same expressions
 */
trait Dsl extends PrimitiveOps with NumericOps with BooleanOps with LiftString with LiftPrimitives
                                with LiftNumeric with LiftBoolean with IfThenElse with Equal with RangeOps
                                with OrderingOps with MiscOps with ArrayOps with StringOps with SeqOps
                                with Functions with While with StaticData with Variables with LiftVariables with ObjectOps
                                // Defined in Twiddle
                                with MathOps with StrOps {}

// DSL Implementation
/*
   IfThenElseExpOpt: to optimize out dead branches
 */
trait DslExp extends Dsl with PrimitiveOpsExpOpt with NumericOpsExpOpt
                          with BooleanOpsExp with IfThenElseExpOpt with EqualExpBridgeOpt with RangeOpsExp
                          with OrderingOpsExp with MiscOpsExp with EffectExp with ArrayOpsExpOpt with StringOpsExp
                          with SeqOpsExp with FunctionsRecursiveExp with WhileExp with StaticDataExp with VariablesExpOpt
                          with ObjectOpsExpOpt
                          // Defined in Twiddle
                          with MathOpsExp with StrOpsExp {}

// DSL C Codegen
trait DslGen extends CGenNumericOps
    with CGenPrimitiveOps with CGenBooleanOps with CGenIfThenElse
    with CGenEqual with CGenRangeOps with CGenOrderingOps
    with CGenMiscOps with CGenArrayOps with CGenStringOps
    with CGenSeqOps with CGenFunctions with CGenWhile
    with CGenStaticData with CGenVariables
    with CGenObjectOps
    // Defined in Twiddle
    with CGenMathOps with CGenStrOps {
  val IR: DslExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait DslImpl extends DslExp { q =>
  val codegen = new DslGen {
    val IR: q.type = q
  }
}

// For testing DSL
abstract class DslSnippet[A:Manifest,B:Manifest] extends Dsl {
  def snippet(x: Rep[A]): Rep[B]
}

abstract class DslDriver[A:Manifest,B:Manifest] extends DslSnippet[A,B] with DslImpl {
  lazy val code: String = {
    implicit val mA = manifestTyp[A]
    implicit val mB = manifestTyp[B]
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))
    source.toString
  }
}