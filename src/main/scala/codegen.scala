package twiddle.dsl

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.math.pow
import sys.process._
import scala.language.postfixOps
import TwiddleAST._
import ParallelAST._
import GeneralAST._
import scala.collection.mutable.HashMap

object CodegenInternal {
  var defines = HashMap[String, String]()
  var lambdaDefs = HashMap[String, Func]()
  var tmpNameCtr = 0
  def eval_node(t: ASTNode): Unit = t match {
    case Num(n) => print(n)
    case CStr(s) => print(s""""$s"""")
    case Bool(b) => if(b) print(1) else print(0)
    case Var(s) => print(s)
    case H(s) => print(s) // Hex
    case F(v) => print("float "); eval_node(v)
    case U(v) => print("unsigned int "); eval_node(v)
    case I(v) => print("int "); eval_node(v)
    case D(v) => print("double "); eval_node(v)
    case S(v) => print("char* "); eval_node(v)
    case Decl(e) => eval_node(e); println(";")
    case Assign(v, e) => e match {
      case Result(Var(vr), exp) => eval_node(exp); eval_node(v); print(" = "); println(vr); println(";")
      case _ => eval_node(v); print(" = "); eval_node(e); println(";")
    }
    case AssignInline(v, e) => eval_node(v); print(" = "); eval_node(e)
    case Cast(c, v) => print("("); eval_node(c); print(")"); eval_node(v)
    case Const(e) => print("const "); eval_node(e)
    case IntPtr() => print("int* ")
    case Addr(e) => print("&("); eval_node(e); print(")")
    case Minus(a, b) => print("("); eval_node(a); print(" - "); eval_node(b); print(")")
    case Times(a, b) => eval_node(a); print(" * "); eval_node(b)
    case Plus(a, b) => (a, b) match {
      // ! extract into `summarizeEffects()` function
      case (Result(v1, t1), Result(v2, t2)) => eval_node(t1); eval_node(t2); eval_node(v1); print(" + "); eval_node(v2)
      case (Result(v, t), exp) => eval_node(t); eval_node(exp); print(" + "); eval_node(v)
      case (exp, Result(v, t)) => eval_node(t); eval_node(exp); print(" + "); eval_node(v)
      case (e1, e2) => eval_node(e1); print(" + "); eval_node(e2)
    }
    case IfThenElse(cond, conseq, alt) =>
      print("if("); eval_node(cond); print(") {\n"); eval_node(conseq); print(";\n} else {\n"); eval_node(alt); println(";\n}")
    case IfThen(cond, conseq) =>
      print("if("); eval_node(cond); print(") {\n"); eval_node(conseq); println(";\n}")
    case TernaryIf(cond, conseq, alt) => print("("); eval_node(cond); print(") ? "); eval_node(conseq); print(" : "); eval_node(alt)
    case RShift(a, b) => print("("); eval_node(a); print(" >> "); eval_node(b); print(")")
    case LShift(a, b) => print("("); eval_node(a); print(" << "); eval_node(b); print(")")
    case Ref(e) => print("*("); eval_node(e); print(")")
    case Tup(hd: Term, tl: Term) => eval_node(hd); eval_node(tl)
    case Null() => ()
    case Eq(e1, e2) => eval_node(e1); print(" == "); eval_node(e2)
    case Gte(e1, e2) => eval_node(e1); print(" >= "); eval_node(e2)
    case Gt(e1, e2) => eval_node(e1); print(" > "); eval_node(e2)
    case Lt(e1, e2) => eval_node(e1); print(" < "); eval_node(e2)
    case Length(s: CStr) => print("strlen("); eval_node(s); print(")")
    case For(init, cond, variant, body) => print("for("); eval_node(init); print(";"); eval_node(cond); print(";"); eval_node(variant); println(")")
                                          println("{"); eval_node(body); println("}")
    case XOR(n1, n2) => eval_node(n1); print(" ^ "); eval_node(n2)
    case PreInc(v) => print("++("); eval_node(v); print(")")
    case PostDec(v) => print("("); eval_node(v); print(")--")
    case Result(v, e) => eval_node(e)
    case SizeOf(a) => print("sizeof("); eval_node(a); print(")");
    case Macro(a) => print(a)
    case RShiftEq(a, b) => eval_node(a); print(" >>= "); eval_node(b)
    case LShiftEq(a, b) => eval_node(a); print(" <<= "); eval_node(b)
    case BitOrEq(n1, n2) => eval_node(n1); print(" |= "); eval_node(n2)
    case BitAnd(n1, n2) => print("("); eval_node(n1); print(" & "); eval_node(n2); print(")")
    case BitOr(n1, n2) => print("("); eval_node(n1); print(" | "); eval_node(n2); print(")")
    case And(a, b) => print("("); eval_node(a); print(" && "); eval_node(b); print(")")
    case Mod(a, b) => print("("); eval_node(a); print(" % "); eval_node(b); print(")")
    case Or(a, b) => print("("); eval_node(a); print(" || "); eval_node(b); print(")")
    case Define(vr, args, vl) => {
      var defStr = s"#define $vr"
      if(args.size != 0) {
        defStr += "("
        defStr += args.mkString(",")
        defStr += ")"
      }
      defStr += s" "
      defStr += vl
      defines += (vr -> defStr)
    }
    case Call(name, args: List[Var]) => {
      val vals = args.map({ v: Var => v.s })
      print(s"""$name(${vals.mkString(",")})""")
    }

    case Printf(f, es) => {
      val Result(Var(varname), exp) = es
      eval_node(exp)
      print(s"printf($f, "); print(varname);println(");")
    }

    case Func(ret, name, params, body) => lambdaDefs += (name -> Func(ret, name, params, body))
    case App(f: Term, t: Term) => f match {
      case func: Func => eval_node(func.body(t))
    }

    case OmpPragma(ps, body) => {
      print("#pragma omp ")
      ps.foreach({ 
        case (name, args) => {
            print(s"$name")
            if(args.size != 0) {
              print("(")
              print(args.mkString(","))
              print(")")
            }
            print(" ")
          }
      })
      println
      eval_node(body)
    }

    case otherwise => println(s"Unknown AST node $otherwise")
  }
}

object Codegen {
  import CodegenInternal._

  def eval(ast: ASTNode): Unit = {
    ast match {
      case Tup(hd: ASTNode, Tup(tl1, tl2)) => eval_node(hd); println(";"); eval(Tup(tl1, tl2))
      case Tup(hd: ASTNode, Null()) => eval_node(hd)
      case Result(v, t) => eval(t)
      case _ => ()
    }
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

  def run(ast: ASTNode): Unit = {
    reset()
    TwiddleAST.reset()
    eval(ast)
  }

  def gensrc(ast: ASTNode): Either[String, Unit] = {
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
      } catch {
        case e: Throwable => println("ERROR occurred during twiddle code generation");
                             Right(e.printStackTrace())
      } finally {
        reset()
        TwiddleAST.reset()
      }
    }
  }

  def reset() = {
    tmpNameCtr = 0
    lambdaDefs.clear()
    defines.clear()
  }

  def runsrc(arg: Either[String, Unit]) = {
    val fileName = arg match {
      case Left(name) => name
      case Right(trace) => trace
    }

    val compileCmd = s"""gcc -g -O0 -fopenmp $fileName -o twiddleOutFile"""
    compileCmd !
    val runCmd = s"./twiddleOutFile"
    runCmd !
  }
}