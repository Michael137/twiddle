// Source: https://github.com/scala-lms/tutorials/blob/master/src/test/scala/lms/tutorial/dslapi.scala

package twiddle.lms.dsl

import scala.lms.common._
import scala.lms.internal._
import scala.reflect.SourceContext
import scala.io._
import java.io.{PrintStream, OutputStream}

trait MathOps extends Base { this: Dsl =>
  def tw_log10[A:Typ](a: Rep[Int]): Rep[Int]
  def tw_log2[A:Typ](a: Rep[Int]): Rep[Int] // TODO: should only operate on floats
}
trait MathOpsExp extends MathOps with BaseExp { this: DslExp =>
  case class Log10[A](a: Rep[Int]) extends Def[Int]
  def tw_log10[A:Typ](a: Exp[Int]): Exp[Int] = Log10(a)
  
  case class Log2[A](a: Rep[Int]) extends Def[Int]
  def tw_log2[A:Typ](a: Exp[Int]): Exp[Int] = Log2(a)
}
trait CGenUtilOps extends CGenBase {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Log10(a) =>
      // emitVarDecl(__newVar(.asInstanceOf[Sym[Variable[Any]]]))
      // emitNode(a.asInstanceOf[Sym[Variable[Any]]], )
      val qa = quote(a)
      emitValDef(sym, s"""($qa >= 1000000000) ? 9 : ($qa >= 100000000) ? 8 : ($qa >= 10000000) ? 7 : 
                          ($qa >= 1000000) ? 6 : ($qa >= 100000) ? 5 : ($qa >= 10000) ? 4 : 
                          ($qa >= 1000) ? 3 : ($qa >= 100) ? 2 : ($qa >= 10) ? 1 : 0;""")
    case Log2(a) =>
      // emitVarDecl(__newVar(.asInstanceOf[Sym[Variable[Any]]]))
      // emitNode(a.asInstanceOf[Sym[Variable[Any]]], )
      val qa = quote(a)
      emitValDef(sym, s"""0""")
      emitAssignment(sym, s"""*(const int *) &${qa};""")
      emitAssignment(sym, s"""(${quote(sym)} >> 23) - 127;""")
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
                                with Functions with While with StaticData with Variables with LiftVariables with ObjectOps with MathOps {
  def generate_comment(l: String): Rep[Unit]
  def comment[A:Typ](l: String, verbose: Boolean = true)(b: => Rep[A]): Rep[A]
}

// DSL Implementation
/*
   IfThenElseExpOpt: to optimize out dead branches
 */
trait DslExp extends Dsl with PrimitiveOpsExpOpt with NumericOpsExpOpt
                          with BooleanOpsExp with IfThenElseExpOpt with EqualExpBridgeOpt with RangeOpsExp
                          with OrderingOpsExp with MiscOpsExp with EffectExp with ArrayOpsExpOpt with StringOpsExp
                          with SeqOpsExp with FunctionsRecursiveExp with WhileExp with StaticDataExp with VariablesExpOpt
                          with ObjectOpsExpOpt with MathOpsExp {
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
    with CGenObjectOps with CGenUtilOps {
  val IR: DslExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IfThenElse(c,Block(Const(true)),Block(Const(false))) =>
      emitValDef(sym, quote(c))
    case PrintF(f:String,xs) =>
      emitValDef(sym, src"printf(${Const(f)::xs})")
    case GenerateComment(s) =>
      stream.println("// "+s)
    case Comment(s, verbose, b) =>
      stream.println("//#" + s)
      if (verbose) {
        stream.println("// generated code for " + s.replace('_', ' '))
      } else {
        stream.println("// generated code")
      }
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("//#" + s)
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