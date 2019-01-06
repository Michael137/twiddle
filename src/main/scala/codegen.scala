package twiddle.dsl

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import scala.collection.BitSet
import scala.language.higherKinds
import scala.math.pow
import scala.language.implicitConversions
import sys.process._
import scala.language.postfixOps

object CodeGen {
  import TwiddleAST._

  // TODO: Tagless interpreter for Twiddle AST
  // ? use LMS (eval.scala in LMS tutorial; Ops[T[_]])
  // either eval/pretty print code or generete C code

  def eval(ast: AST[_]): Unit = {
    ast match {
      case Tup(hd: Term, Tup(tl1, tl2)) => eval_term(hd); println(";"); eval(Tup(tl1, tl2))
      case Tup(hd: Term, Null()) => eval_term(hd)
      case Result(v, t) => eval(t)
      case _ => ()
    }
  }
  
  import scala.collection.mutable.HashMap
  var defines = HashMap[String, String]()
  var lambdaDefs = HashMap[String, Func]()
  var tmpNameCtr = 0
  def eval_term(t: Term): Unit = t match {
    case Num(n) => print(n)
    case CStr(s) => print(s""""$s"""")
    case Bool(b) => if(b) print(1) else print(0)
    case Var(s) => print(s)
    case H(s) => print(s)
    case Decl(e) => e match {
      case F(v) => print("float "); eval_term(v); println(";")
      case U(v) => print("unsigned int "); eval_term(v); println(";")
      case I(v) => print("int "); eval_term(v); println(";")
      case D(v) => print("double "); eval_term(v); println(";")
      case S(v) => print("char* "); eval_term(v); println(";")
    }
    case Assign(v, e) => e match {
      case Result(Var(vr), exp) => eval_term(exp); eval_term(v); print(" = "); println(vr); println(";")
      case _ => eval_term(v); print(" = "); eval_term(e); println(";")
    }
    case Cast(c, v) => print("("); eval_term(c); print(")"); eval_term(v)
    case Const(e) => print("const "); eval_term(e)
    case IntPtr() => print("int* ")
    case Addr(e) => print("&("); eval_term(e); print(")")
    case Minus(a, b) => eval_term(a); print(" - "); eval_term(b)
    case Times(a, b) => eval_term(a); print(" * "); eval_term(b)
    case Plus(a, b) => (a, b) match {
      // ! extract into `summarizeEffects()` function
      case (Result(v1, t1), Result(v2, t2)) => eval_term(t1); eval_term(t2); eval_term(v1); print(" + "); eval_term(v2)
      case (Result(v, t), exp) => eval_term(t); eval_term(exp); print(" + "); eval_term(v)
      case (exp, Result(v, t)) => eval_term(t); eval_term(exp); print(" + "); eval_term(v)
      case (e1, e2) => eval_term(e1); print(" + "); eval_term(e2)
    }
    case IfThenElse(cond, conseq, alt) =>
      print("if("); eval_term(cond); print(") {\n"); eval_term(conseq); print(";\n} else {\n"); eval_term(alt); println(";\n}")
    case IfThen(cond, conseq) =>
      print("if("); eval_term(cond); print(") {\n"); eval_term(conseq); println(";\n}")
    case TernaryIf(cond, conseq, alt) => print("("); eval_term(cond); print(") ? "); eval_term(conseq); print(" : "); eval_term(alt)
    case RShift(a, b) => print("("); eval_term(a); print(" >> "); eval_term(b); print(")")
    case LShift(a, b) => print("("); eval_term(a); print(" << "); eval_term(b); print(")")
    case Ref(e) => print("*("); eval_term(e); print(")")
    case Tup(hd: Term, tl: Term) => eval_term(hd); eval_term(tl)
    case Null() => ()
    case Gte(e1, e2) => eval_term(e1); print(" >= "); eval_term(e2)
    case Gt(e1, e2) => eval_term(e1); print(" > "); eval_term(e2)
    case Length(s: CStr) => print("strlen("); eval_term(s); print(")")
    case For(init, cond, variant, body) => print("for("); eval_term(init); print(";"); eval_term(cond); print(";"); eval_term(variant); println(")")
                                          println("{"); eval_term(body); println("}")
    case XOR(n1, n2) => eval_term(n1); print(" ^ "); eval_term(n2)
    case PreInc(v) => print("++("); eval_term(v); print(")")
    case PostDec(v) => print("("); eval_term(v); print(")--")
    case Result(v, e) => eval_term(e)
    case SizeOf(a) => print("sizeof("); eval_term(a); print(")");
    case Macro(a) => print(a)
    case RShiftEq(a, b) => eval_term(a); print(" >>= "); eval_term(b)
    case LShiftEq(a, b) => eval_term(a); print(" <<= "); eval_term(b)
    case BitOrEq(n1, n2) => eval_term(n1); print(" |= "); eval_term(n2)
    case BitAnd(n1, n2) => print("("); eval_term(n1); print(" & "); eval_term(n2); print(")")
    case BitOr(n1, n2) => print("("); eval_term(n1); print(" | "); eval_term(n2); print(")")
    case And(a, b) => print("("); eval_term(a); print(" && "); eval_term(b); print(")")
    case Or(a, b) => print("("); eval_term(a); print(" || "); eval_term(b); print(")")
    case Define(vr, args, vl) =>
      var defStr = s"#define $vr"
      if(args.size != 0) {
        defStr += "("
        defStr += args.mkString(",")
        defStr += ")"
      }
      defStr += s" "
      defStr += vl
      defines += (vr -> defStr)
    case Call(name, args: List[Var]) =>
      val vals = args.map({ v: Var => v.s })
      print(s"""$name(${vals.mkString(",")})""")
    case Printf(f, es) =>
      val varnames = es.map({ t: Term => t match {
          case Result(vr, exp) => eval_term(exp);vr.s
          case tup: Tup => val tmpVarName = s"tmp${tmpNameCtr}";
                                   tmpNameCtr+=1;
                                   eval_term(Assign(Var(tmpVarName), tup)); tmpVarName
        }
      })
      print(s"printf($f "); varnames.foreach({s => print(s", $s") }); println(");")

    case Func(ret, name, params, body) => lambdaDefs += (name -> Func(ret, name, params, body))
    case App(f: Term, t: Term) => f match {
      case func: Func => eval_term(func.body(t))
    }

    case otherwise => println(s"Unknown AST node $otherwise")
  }

  def emitGenHeader() = println("// START OF CODE GENERATED BY TWIDDLE")
  def emitGenFooter() = println("// END OF CODE GENERATED BY TWIDDLE")
  
  def emitIncludes() = {
    val incls = List("math.h", "stdlib.h", "stdio.h", "string.h", "limits.h")
    incls.foreach({ s => println(s"#include <$s>")})
  }

  def emitDefines() = {
    defines.foreach( x => println(x._2))
  }

  def emitMainBegin() = {
    println("int main() {")
  }

  def emitMainEnd() = {
    println("\n; return 0;\n}")
  }

  def gensrc(ast: AST[_]): Either[String, Unit] = {
    import java.io._

    val (filename, program) = ast match {
      case Program(name: String, rst) => (name, rst)
      case _ => ("twiddle.c", ast)
    }

    "rm -f twiddleOutFile" !
    val cleanCmd = s"rm -f $filename"
    cleanCmd !

    println(s"// GENERATING to file: $filename")
    val out_file = new FileOutputStream(new File(filename))
    val evaled_prog = new ByteArrayOutputStream()
    scala.Console.withOut(out_file) {
      try{
        scala.Console.withOut(evaled_prog) { eval(program) } // To collect defines and definitions

        emitGenHeader()
        emitIncludes()
        emitDefines()
        // emitFunDefs() // Function definitions
        emitMainBegin()
        println(evaled_prog)
        emitMainEnd()
        emitGenFooter()
        Left(filename)
      } catch { case e: Throwable => println("ERROR occurred during twiddle code generation");Right(e.printStackTrace()) }
    }
  }

  def runsrc(arg: Either[String, Unit]) = {
    val fileName = arg match {
      case Left(name) => name
      case Right(trace) => trace
    }

    val compileCmd = s"""gcc -g -O0 $fileName -o twiddleOutFile"""
    compileCmd !
    val runCmd = s"./twiddleOutFile"
    runCmd !
  }
}