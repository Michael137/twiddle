package twiddle.dsl

import scala.lms.common._
import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect._
import scala.collection.BitSet
import scala.language.higherKinds
import scala.math.pow
import scala.language.implicitConversions

object CodeGen {
  import Syntax._

  type AST[A] = Term
  abstract class Term
  case class IntPtr() extends Term
  case class Const(e: Term) extends Term
  case class Addr(e: Term) extends Term
  case class Var(s: String) extends Term
  case class Num[A](n: A) extends Term
  case class Bool(b: Boolean) extends Term
  case class CStr(s: String) extends Term // C-style string
  case class Gte(e1: Term, e2: Term) extends Term
  case class Gt(e1: Term, e2: Term) extends Term
  case class PreInc(e1: Term) extends Term
  case class Assign(v: Term, e: Term) extends Term
  case class Decl(v: Term) extends Term
  case class I(v: Var) extends Term // Integer
  case class F(v: Var) extends Term // Float
  case class D(v: Var) extends Term // Double
  case class S(v: Var) extends Term // String
  case class Rshift(v: Term, n: Term) extends Term // >>
  case class Minus(v: Term, n: Term) extends Term // -
  case class Plus(v: Term, n: Term) extends Term // +
  case class Times(v: Term, n: Term) extends Term // *
  case class Divide(v: Term, n: Term) extends Term // /
  case class Mod(v: Term, n: Term) extends Term // /
  case class XOR(n1: Term, n2: Term) extends Term // ^
  case class Cast(c: Term, e: Term) extends Term
  case class IfThenElse(cond: Term, conseq: Term, alt: Term) extends Term
  case class TernaryIf(cond: Term, conseq: Term, alt: Term) extends Term // TODO: DRY
  case class Ref(e: Term) extends Term // pointer dereference
  case class Null() extends Term // null type
  case class Length(s: CStr) extends Term // null type
  case class For(init: Term, cond: Term, variant: Term, body: Term) extends Term // null type

  case class Tup(e1: Any, e2: Any) extends Term // tuple // ! should be Tup(e1: Term, e2: Term)

  case class Result(v: Var, e: Term) extends Term
  
  implicit object EmitTwiddleAST extends CExp[AST] {
    def num[A](v: A) = Tup(Num(v), Null())
    def bool(b: Boolean) = Tup(Bool(b), Null())
    def ifThenElse_[A] : AST[_] => (() => AST[_]) => (() => AST[_]) => AST[_] =
            b => t => e => Tup(IfThenElse(b, t(), e()), Null())
           
    def add[A: Numeric](a: AST[A], b: AST[A]): AST[A] = Tup(Plus(a, b), Null())
    def sub[A: Numeric](a: AST[A], b: AST[A]): AST[A] = Tup(Minus(a, b), Null())
    def mul[A: Numeric](a: AST[A], b: AST[A]): AST[A] = Tup(Times(a, b), Null())
    def div(a: AST[Int], b: AST[Int]): AST[Int] = Tup(Divide(a, b), Null())
    def mod(a: AST[Int], b: AST[Int]): AST[Int] =  Tup(Mod(a, b), Null())
    def cons(a: Any, b: Any) = Tup(a, b)
    def car(t: AST[(AST[Any], AST[Any])]): AST[Any] = {
      val Tup(a: Term, _) = t
      a
    }
    def cdr(t: AST[(AST[Any], AST[Any])]): AST[Any] = {
      val Tup(_, b: Term) = t
      b
    }

    def log2(a: AST[Int]): AST[Int] = {
      val Tup(t: Term, Null()) = a
      Result(Var("x1"),
      Tup(Decl(F(Var("x0"))),
      Tup(Decl(I(Var("x1"))),
      Tup(Assign(Var("x0"), t),
      Tup(Assign(Var("x1"), Ref(Cast(Const(IntPtr()), Addr(Var("x0"))))),
      Tup(Assign(Var("x1"), Minus(Rshift(Var("x1"), Num(23)), Num(127))), Null()))))))
    }

    def log10(a: AST[Int]): AST[Int] = {
      val Tup(t: Term, Null()) = a
      Result(Var("x1"),
      Tup(Decl(I(Var("x1"))),
      Tup(Assign(Var("x1"), TernaryIf(Gte(t, Num(1000000000)), Num(9),
                            TernaryIf(Gte(t, Num(100000000)), Num(8),
                            TernaryIf(Gte(t, Num(10000000)), Num(7),
                            TernaryIf(Gte(t, Num(1000000)), Num(6),
                            TernaryIf(Gte(t, Num(100000)), Num(5),
                            TernaryIf(Gte(t, Num(10000)), Num(4),
                            TernaryIf(Gte(t, Num(1000)), Num(3),
                            TernaryIf(Gte(t, Num(100)), Num(2),
                            TernaryIf(Gte(t, Num(10)), Num(1),
                            Num(0))))))))))), Null())))
    }

    
    def ternaryIf[A] : AST[_] => (() => AST[_]) => (() => AST[_]) => AST[_] =
            b => t => e => Tup(TernaryIf(b, t(), e()), Null())

    def string(s: String): AST[String] = Tup(CStr(s), Null())

    def reverse(a: AST[String]): AST[String] = {
      val Tup(s: CStr, _) = a
      Result(Var("str"),
        Tup(Decl(S(Var("p1"))),
        Tup(Decl(S(Var("p2"))),
        Tup(Assign(Var("p1"), s),
        Tup(Assign(Var("p2"), Plus(s, Minus(Length(s), Num(1)))),
        Tup(For(Null(), Gt(Var("p2"), Var("p1")), PreInc(Var("p1")),
                Tup(Assign(Ref(Var("p1")), XOR(Ref(Var("p1")),Ref(Var("p2")))),
                Tup(Assign(Ref(Var("p2")), XOR(Ref(Var("p2")),Ref(Var("p1")))),
                Tup(Assign(Ref(Var("p1")), XOR(Ref(Var("p1")),Ref(Var("p2")))),
                Tup(Assign(Var("p2"), Minus(Var("p2"), Num(1))), Null()))))), Null()))))))
    }
  }

  // TODO: Tagless interpreter for Twiddle AST
  // ? use LMS (eval.scala in LMS tutorial; Ops[T[_]])
  // either eval/pretty print code or generete C code

  def emitGenHeader() = println("// START OF CODE GENERATED BY TWIDDLE")
  def emitGenFooter() = println("// END OF CODE GENERATED BY TWIDDLE")

  // Multi-stage evaluator
  def eval(ast: AST[_]): Unit = {
    ast match {
      case Tup(hd: Term, Tup(tl1, tl2)) => eval_term(hd); eval(Tup(tl1, tl2))
      case Tup(hd: Term, Null()) => eval_term(hd)
      case Result(v, t) => eval(t)
      case _ => ()
    }
  }

  def eval_term(t: Term): Unit = t match {
    case Num(n) => print(n)
    case CStr(s) => print(s""""$s"""")
    case Bool(b) => if(b) print(1) else print(0)
    case Var(s) => print(s)
    case Decl(e) => e match {
      case F(v) => print("float "); eval_term(v); println(";")
      case I(v) => print("int "); eval_term(v); println(";")
      case D(v) => print("double "); eval_term(v); println(";")
      case S(v) => print("char* "); eval_term(v); println(";")
    }
    case Assign(v, e) => eval_term(v); print(" = "); eval_term(e); println(";")
    case Cast(c, v) => print("("); eval_term(c); print(")"); eval_term(v)
    case Const(e) => print("const "); eval_term(e)
    case IntPtr() => print("int* ")
    case Addr(e) => print("&("); eval_term(e); print(")")
    case Minus(a, b) => eval_term(a); print(" - "); eval_term(b)
    case Plus(a, b) => (a, b) match {
      // ! extract into `summarizeEffects()` function
      case (Result(v1, t1), Result(v2, t2)) => eval_term(t1); eval_term(t2); eval_term(v1); print(" + "); eval_term(v2)
      case (Result(v, t), exp) => eval_term(t); eval_term(exp); print(" + "); eval_term(v)
      case (exp, Result(v, t)) => eval_term(t); eval_term(exp); print(" + "); eval_term(v)
      case (e1, e2) => eval_term(e1); print(" + "); eval_term(e2)
    }
    case IfThenElse(cond, conseq, alt) => print("if("); eval_term(cond); print(") {"); eval_term(conseq); print("} else {"); eval_term(alt); println("}")
    case TernaryIf(cond, conseq, alt) => print("("); eval_term(cond); print(") ? "); eval_term(conseq); print(" : "); eval_term(alt)
    case Rshift(a, b) => print("("); eval_term(a); print(" >> "); eval_term(b); print(")")
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
    case otherwise => println(s"Unknown AST node $otherwise")
  }
}