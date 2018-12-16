// Source: https://github.com/scala-lms/tutorials/blob/master/src/test/scala/lms/tutorial/dslapi.scala

package twiddle.lms.dsl

import scala.lms.common._
import scala.lms.internal._
import scala.reflect.SourceContext
import scala.io._
import java.io.{PrintStream, OutputStream}

// DSL Interface
/*
  LiftVariables: to stage variables i.e. be able to use pass variables to staged code
  LiftNumeric: to allow mixed non-Rep and Rep integers in same expressions
 */
trait Dsl extends ScalaOpsPkg with LiftVariables with LiftNumeric {
  def generate_comment(l: String): Rep[Unit]
  def comment[A:Typ](l: String, verbose: Boolean = true)(b: => Rep[A]): Rep[A]
}

// DSL Implementation
/*
   IfThenElseExpOpt: to optimize out dead branches
 */
trait DslExp extends Dsl with ScalaOpsPkgExp {
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
trait DslGen extends CCodeGenPkg {
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