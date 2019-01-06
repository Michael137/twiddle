package twiddle.dsl

import scala.reflect._
import scala.reflect.runtime.universe._
import scala.language.implicitConversions

object TwiddleAST {
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
    case class H(s: String) extends Term // Hex number
    case class B(s: String) extends Term // Bit number
    case class F(v: Var) extends Term // Float
    case class D(v: Var) extends Term // Double
    case class S(v: Var) extends Term // String
    case class U(v: Var) extends Term // Unsigned Integer (also used to represent bits)
    case class RShift(v: Term, n: Term) extends Term // >>
    case class LShift(v: Term, n: Term) extends Term // <<
    case class Minus(v: Term, n: Term) extends Term // -
    case class Plus(v: Term, n: Term) extends Term // +
    case class Times(v: Term, n: Term) extends Term // *
    case class Divide(v: Term, n: Term) extends Term // /
    case class Mod(v: Term, n: Term) extends Term // /
    case class XOR(n1: Term, n2: Term) extends Term // ^
    case class Cast(c: Term, e: Term) extends Term
    case class IfThenElse(cond: Term, conseq: Term, alt: Term) extends Term
    case class IfThen(cond: Term, conseq: Term) extends Term
    case class TernaryIf(cond: Term, conseq: Term, alt: Term) extends Term // TODO: DRY
    case class Ref(e: Term) extends Term // pointer dereference
    case class Null() extends Term // null type
    case class Length(s: CStr) extends Term // null type
    case class For(init: Term, cond: Term, variant: Term, body: Term) extends Term // null type
    case class Printf(format: String, vars: List[Term]) extends Term
    case class SizeOf(v: Term) extends Term
    case class Macro(s: String) extends Term
    case class RShiftEq(a: Term, b: Term) extends Term
    case class LShiftEq(a: Term, b: Term) extends Term
    case class BitOrEq(a: Term, b: Term) extends Term
    case class BitOr(a: Term, b: Term) extends Term
    case class BitAnd(a: Term, b: Term) extends Term
    case class Or(a: Term, b: Term) extends Term
    case class And(a: Term, b: Term) extends Term
    case class PostDec(a: Term) extends Term
    case class Func(ret: String, name: String, params: List[String], body: Term => Term) extends Term
    case class App(f: Term, arg: Term) extends Term // f can be Var or Func
    case class Define(vr: String, args: List[String], vl: String) extends Term // TODO: Could be improved by taking Term instead of simple String
    case class Call(f: String, arg: List[Var]) extends Term // TODO: f should be Var

    case class Tup(e1: Any, e2: Any) extends Term // tuple // ! should be Tup(e1: Term, e2: Term)

    case class Program(name: String, e: Term) extends Term
    case class Result(v: Var, e: Term) extends Term

    import scala.reflect._
    import Syntax._

    implicit object EmitTwiddleAST extends Exp[AST] {
        var varCtr = 0
        def fresh() = { varCtr += 1; varCtr }
        def gensym(varName: String) = { s"$varName${fresh()}" }
        def reset() = { varCtr = 0 }

        // We represent bits as unsigned integers in the generated C.
        // At this level (in Scala) its just another integer.
        // Bit operations in the twiddle IR are responsible
        // for declaring 'Num's as 'U's
        implicit def d2i(x: Term): Term = x
        def bits(a: AST[Int]): Term = a
        def reverseBitsParallel(b: Term): Term = {
            val Tup(t: Term, Null()) = b
            val v = gensym("v")
            Result(Var(v),
                    Tup(Decl(U(Var(v))),
                    Tup(Assign(Var(v), t),
                    Tup(Assign(Var(v), BitOr(BitAnd(RShift(Var(v), Num(1)), H("0x55555555")),
                                             LShift(BitAnd(Var(v), H("0x55555555")), Num(1)))),
                    Tup(Assign(Var(v), BitOr(BitAnd(RShift(Var(v), Num(2)), H("0x33333333")),
                                             LShift(BitAnd(Var(v), H("0x33333333")), Num(2)))),
                    Tup(Assign(Var(v), BitOr(BitAnd(RShift(Var(v), Num(4)), H("0x0F0F0F0F")),
                                             LShift(BitAnd(Var(v), H("0x0F0F0F0F")), Num(4)))),
                    Tup(Assign(Var(v), BitOr(BitAnd(RShift(Var(v), Num(8)), H("0x00FF00FF")),
                                             LShift(BitAnd(Var(v), H("0x00FF00FF")), Num(8)))),
                    Tup(Assign(Var(v), BitOr(RShift(Var(v), Num(16)),
                                             LShift(Var(v),Num(16)))),
                    Null()))))))))
        }
        def reverseBits(b: Term): Term = {
            val Tup(t: Term, Null()) = b
            val r = gensym("r")
            val s = gensym("s")
            val v = gensym("v")

            Result(Var(r),
                    Tup(Decl(U(Var(v))),
                    Tup(Decl(U(Var(r))),
                    Tup(Decl(I(Var(s))),
                    Tup(Assign(Var(v), t),
                    Tup(Assign(Var(r), t),
                    Tup(Assign(Var(s), Minus(Times(SizeOf(Var(v)), Macro("CHAR_BIT")), Num(1))),
                    Tup(For(RShiftEq(Var(v), Num(1)), Var(v), RShiftEq(Var(v), Num(1)),
                            Tup(LShiftEq(Var(r), Num(1)),
                            Tup(BitOrEq(Var(r), BitAnd(Var(v), Num(1))),
                            Tup(PostDec(Var(s)), Null())))),
                    Tup(LShiftEq(Var(r), Var(s)), Null())))))))))
        }

        def hasZero(b: Term): Term = {
            val Tup(t: Term, Null()) = b
            val v = gensym("v")
            val r = gensym("r")

            Result(Var(r),
                Tup(Decl(U(Var(r))),
                Tup(Decl(U(Var(v))),
                Tup(Assign(Var(v), t),
                Tup(Define("HAS_ZERO", List("v"), "(((v) - 0x01010101UL) & ~(v) & 0x80808080UL)"),
                Tup(Assign(Var(r), Call("HAS_ZERO", List(Var(v)))), Null()))))))
        }

        def swapBits(a: Term, b: Term): Term = {
            val (t1, t2) = (a, b) match {
                case (Tup(t1: Term, Null()), Tup(t2: Term, Null())) => (t1, t2)
                case (Tup(t1: Term, Null()), r: Result) => (t1, r)
            }
            val v1 = gensym("v")
            val v2 = gensym("v")
            val r = gensym("r")

            Result(Var(r),
                Tup(Decl(U(Var(r))),
                Tup(Decl(U(Var(v1))),
                Tup(Decl(U(Var(v2))),
                Tup(Assign(Var(v1), t1),
                Tup(Assign(Var(v2), t2),
                Tup(Define("SWAP", List("a, b"), "(((a) ^= (b)), ((b) ^= (a)), ((a) ^= (b)))"),
                Tup(Assign(Var(r), Call("SWAP", List(Var(v1), Var(v2)))), Null()))))))))
        }

        // Function definitions collected in code generator
        def lam[A: ClassTag, B: ClassTag](f: AST[A] => AST[B]): Term = {
            val funcName = gensym("func")
            val paramType = classTag[A].runtimeClass.toString
            val retType = classTag[B].runtimeClass.toString

            /*import scala.tools.reflect.ToolBox
            import scala.reflect.runtime.{currentMirror => m}
            val tb = m.mkToolBox()
            println(tb.parse("{ x => add(x, 1) }"))*/

            Tup(Func(retType, funcName, List(paramType), f), Null())
            // S(Var(funcName))
        }

        def app[A, B] = (f: AST[A => B]) => (arg: AST[A]) => {
            val Tup(func: Func, _) = f
            Tup(App(func, arg), Null())
        }

        def num[A](v: A) = Tup(Num(v), Null())
        def bool(b: Boolean) = Tup(Bool(b), Null())

        val IntClass = classTag[Int]
        val DoubleClass = classTag[Double]
        val StringClass = classTag[String]
        val FloatClass = classTag[Float]
        def ifThenElse[A](b: AST[Boolean])(t: (() => AST[A]))(e: (() => AST[A]))(implicit tag: ClassTag[A]): AST[A] =
        {
            val cond = gensym("cond")
            val conseq = gensym("cons")
            val alt = gensym("alt")
            val (conseqAST, altAST) = tag match {
                case IntClass => (I(Var(conseq)), I(Var(alt)))
                case DoubleClass => (D(Var(conseq)), D(Var(alt)))
                case FloatClass => (F(Var(conseq)), F(Var(alt)))
                case StringClass => (S(Var(conseq)), S(Var(alt)))
                case _ => (U(Var(conseq)), U(Var(alt)))
            }
            Tup(Decl(I(Var(cond))),
            Tup(Decl(conseqAST),
            Tup(Decl(altAST),
            Tup(Assign(Var(cond), b),
            Tup(IfThenElse(Var(cond), Assign(Var(conseq), t()), Assign(Var(alt), e())), Null())))))
        }

        def ifThen[A](b: AST[Boolean])(t: (() => AST[A]))(implicit tag: ClassTag[A]): AST[Unit] =
        {
            val cond = gensym("cond")
            val conseq = gensym("cons")
            val conseqAST = tag match {
                case IntClass => I(Var(conseq))
                case DoubleClass => D(Var(conseq))
                case FloatClass => F(Var(conseq))
                case StringClass => S(Var(conseq))
                case _ => U(Var(conseq))
            }
            Tup(Decl(I(Var(cond))),
            Tup(Decl(conseqAST),
            Tup(Assign(Var(cond), b),
            Tup(IfThen(Var(cond), Assign(Var(conseq), t())), Null()))))
        }

        def add[A: Numeric](a: AST[A], b: AST[A]): AST[A] = Tup(Plus(a, b), Null())
        def sub[A: Numeric](a: AST[A], b: AST[A]): AST[A] = Tup(Minus(a, b), Null())
        def mul[A: Numeric](a: AST[A], b: AST[A]): AST[A] = Tup(Times(a, b), Null())
        def div(a: AST[Int], b: AST[Int]): AST[Int] = Tup(Divide(a, b), Null())
        def mod(a: AST[Int], b: AST[Int]): AST[Int] =  Tup(Mod(a, b), Null())
        def cons(a: Any, b: Any) = Tup(a, b)
        def car(t: Any): Any = {
            val Tup(a: Term, _) = t
            a
        }
        def cdr(t: Any): Any = {
            val Tup(_, b: Term) = t
            b
        }

        def log2(a: AST[Int]): AST[Int] = {
            val Tup(t: Term, Null()) = a
            val x0 = gensym("x")
            val x1 = gensym("ret")

            Result(Var(x1),
            Tup(Decl(F(Var(x0))),
            Tup(Decl(I(Var(x1))),
            Tup(Assign(Var(x0), t),
            Tup(Assign(Var(x1), Ref(Cast(Const(IntPtr()), Addr(Var(x0))))),
            Tup(Assign(Var(x1), Minus(RShift(Var(x1), Num(23)), Num(127))), Null()))))))
        }

        def log10(a: AST[Int]): AST[Int] = {
            val Tup(t: Term, Null()) = a
            val x1 = gensym("ret")

            Result(Var(x1),
            Tup(Decl(I(Var(x1))),
            Tup(Assign(Var(x1), TernaryIf(Gte(t, Num(1000000000)), Num(9),
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

        def begin[A](as: List[AST[A]]): AST[A] = {
            as.foldRight(Tup(Null(), Null()))({ (e1, e2) => Tup(e1, e2)})
        }

        def prints[A](format: String, es: List[AST[A]]) = {
            Tup(Printf(format, es), Null())
        }

        def and(a: AST[Boolean], b: AST[Boolean]): AST[Boolean] = And(a, b)
        def or(a: AST[Boolean], b: AST[Boolean]): AST[Boolean] = Or(a, b)
    }
}