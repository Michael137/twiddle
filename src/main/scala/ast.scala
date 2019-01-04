package twiddle.dsl

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
    case class F(v: Var) extends Term // Float
    case class D(v: Var) extends Term // Double
    case class S(v: Var) extends Term // String
    case class U(v: Var) extends Term // Unsigned Integer (also used to represent bits)
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
    case class Printf(format: String, vars: List[Term]) extends Term
    case class SizeOf(v: Term) extends Term
    case class Macro(s: String) extends Term
    case class RShiftEq(a: Term, b: Term) extends Term
    case class LShiftEq(a: Term, b: Term) extends Term
    case class BitOrEq(a: Term, b: Term) extends Term
    case class BitAnd(a: Term, b: Term) extends Term
    case class PostDec(a: Term) extends Term
    case class Func(ret: String, name: String, params: List[String], body: Term => Term) extends Term
    case class App(f: Term, arg: Term) extends Term // f can be Var or Func

    case class Tup(e1: Any, e2: Any) extends Term // tuple // ! should be Tup(e1: Term, e2: Term)

    case class Program(name: String, e: Term) extends Term
    case class Result(v: Var, e: Term) extends Term

    import scala.reflect._
    import Syntax._

    implicit object EmitTwiddleAST extends Exp[AST] {
        var stFresh = 0
        def fresh() = { stFresh += 1; stFresh }
        def gensym(varName: String) = { s"$varName${fresh()}" }

        // We represent bits as unsigned integers in the generated C.
        // At this level (in Scala) its just another integer.
        // Bit operations in the twiddle IR are responsible
        // for declaring 'Num's as 'U's
        def bits(a: Int): Term = num(a)
        def reverseBitsParallel(b: Term): Term = {
            val Tup(t: Term, Null()) = b
            val v = gensym("v")
            // Result(Var(v),
            //         Tup(Decl(U(Var(v))),
            //         Tup(Assign(Var(v), t),
            //         Tup(Assign(Var(v), )
            Null() // TODO: 
                   /* unsigned int v; // 32-bit word to reverse bit order
                    v = ((v >> 1) & 0x55555555) | ((v & 0x55555555) << 1);
                    v = ((v >> 2) & 0x33333333) | ((v & 0x33333333) << 2);
                    v = ((v >> 4) & 0x0F0F0F0F) | ((v & 0x0F0F0F0F) << 4);
                    v = ((v >> 8) & 0x00FF00FF) | ((v & 0x00FF00FF) << 8);
                    v = ( v >> 16             ) | ( v               << 16); */
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
            val x0 = gensym("x")
            val x1 = gensym("ret")

            Result(Var(x1),
            Tup(Decl(F(Var(x0))),
            Tup(Decl(I(Var(x1))),
            Tup(Assign(Var(x0), t),
            Tup(Assign(Var(x1), Ref(Cast(Const(IntPtr()), Addr(Var(x0))))),
            Tup(Assign(Var(x1), Minus(Rshift(Var(x1), Num(23)), Num(127))), Null()))))))
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
    }
}