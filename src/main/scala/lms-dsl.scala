// Source: https://github.com/scala-lms/tutorials/blob/master/src/test/scala/lms/tutorial/dslapi.scala

package twiddle.lms.dsl

import scala.lms.common._
import scala.lms.internal._
import scala.reflect.SourceContext
import scala.io._
import java.io.{PrintStream, OutputStream}

// DSL Interface
trait Dsl extends PrimitiveOps with NumericOps with BooleanOps
                              with LiftString with LiftPrimitives with LiftNumeric
                              with LiftBoolean with IfThenElse with Equal
                              with RangeOps with OrderingOps with MiscOps
                              with ArrayOps with StringOps with SeqOps
                              with Functions with While with StaticData
                              with Variables with LiftVariables with ObjectOps {
  override def infix_&&(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: scala.reflect.SourceContext): Rep[Boolean] =
    __ifThenElse(lhs, rhs, unit(false))
  def generate_comment(l: String): Rep[Unit]
  def comment[A:Typ](l: String, verbose: Boolean = true)(b: => Rep[A]): Rep[A]
}

// DSL Implementation
trait DslExp extends Dsl with PrimitiveOpsExpOpt with NumericOpsExpOpt with BooleanOpsExp
                        with IfThenElseExpOpt with EqualExpBridgeOpt with RangeOpsExp
                        with OrderingOpsExp with MiscOpsExp with EffectExp
                        with ArrayOpsExpOpt with StringOpsExp with SeqOpsExp
                        with FunctionsRecursiveExp with WhileExp with StaticDataExp
                        with VariablesExpOpt with ObjectOpsExpOpt {
  override def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = lhs match {
    case Const(false) => rhs
    case _ => super.boolean_or(lhs, rhs)
  }
  override def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = lhs match {
    case Const(true) => rhs
    case _ => super.boolean_and(lhs, rhs)
  }

  case class GenerateComment(l: String) extends Def[Unit]
  def generate_comment(l: String) = reflectEffect(GenerateComment(l))

  case class Comment[A:Typ](l: String, verbose: Boolean, b: Block[A]) extends Def[A]
  def comment[A:Typ](l: String, verbose: Boolean)(b: => Rep[A]): Rep[A] = {
    val br = reifyEffects(b)
    val be = summarizeEffects(br)
    reflectEffect[A](Comment(l, verbose, br), be)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Comment(_, _, b) => effectSyms(b)
    case _ => super.boundSyms(e)
  }
}

// DSL C Codegen
trait DslGen extends CGenNumericOps
  with CGenPrimitiveOps with CGenBooleanOps with CGenIfThenElse
  with CGenEqual with CGenRangeOps with CGenOrderingOps
  with CGenMiscOps with CGenArrayOps with CGenStringOps
  with CGenSeqOps with CGenFunctions with CGenWhile
  with CGenStaticData with CGenVariables
  with CGenObjectOps {
  val IR: DslExp

  import IR._

  override def quote(x: Exp[Any]) = x match {
    case Const('\n') if x.tp == typ[Char] => "'\\n'"
    case Const('\t') if x.tp == typ[Char] => "'\\t'"
    case Const(0)    if x.tp == typ[Char] => "'\\0'"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IfThenElse(c,Block(Const(true)),Block(Const(false))) =>
      emitValDef(sym, quote(c))
    case PrintF(f:String,xs) =>
      emitValDef(sym, src"printf(${Const(f)::xs})")
    case GenerateComment(s) =>
      stream.println("// "+s)
    case Comment(s, verbose, b) =>
      stream.println("val " + quote(sym) + " = {")
      stream.println("//#" + s)
      if (verbose) {
        stream.println("// generated code for " + s.replace('_', ' '))
      } else {
        stream.println("// generated code")
      }
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("//#" + s)
      stream.println("}")
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