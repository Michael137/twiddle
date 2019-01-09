package twiddle.dsl

import Syntax._
import TwiddleAST._
import ParallelAST._
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

object AstInterpreter {
    implicit object EmitTwiddleAST extends Exp[AST] {
        val IntClass = classTag[Int]
        val DoubleClass = classTag[Double]
        val StringClass = classTag[String]
        val FloatClass = classTag[Float]
        val BooleanClass = classTag[Boolean]
        
        def classTagToVarNode[A](tag: ClassTag[A]) = {
            tag match {
                case IntClass => { s: String => I(Var(s)) }
                case BooleanClass => { s: String => I(Var(s)) }
                case DoubleClass => { s: String => D(Var(s)) }
                case FloatClass => { s: String => F(Var(s)) }
                case StringClass => { s: String => S(Var(s)) }
                case _ => { s: String => U(Var(s)) }
            }
        }

        // We represent bits as unsigned integers in the generated C.
        // At this level (in Scala) its just another integer.
        // Bit operations in the twiddle IR are responsible
        // for declaring 'Num's as 'U's
        implicit def d2i(x: Term): Term = x
        def null_() = Null()
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

        // 32-bit
        def bitParityParallel(bits: Term): Term = {
            val Tup(t: Term, Null()) = bits
            val v = gensym("v");
            val res = gensym("res");
            Result(Var(res),
                    Tup(Decl(U(Var(v))),
                    Tup(Decl(U(Var(res))),
                    Tup(Assign(Var(v), t),
                    Tup(Assign(Var(v), XOR(Var(v), RShift(Var(v), Num(16)))),
                    Tup(Assign(Var(v), XOR(Var(v), RShift(Var(v), Num(8)))),
                    Tup(Assign(Var(v), XOR(Var(v), RShift(Var(v), Num(4)))),
                    Tup(Assign(Var(v), BitAnd(Var(v), H("0xf"))),
                    Tup(Assign(Var(res), BitAnd(RShift(H("0x6996"), Var(v)), Num(1))),
                    Null())))))))))
        }

        // 32-bit
        def bitParity(bits: Term): Term = {
            val Tup(t: Term, Null()) = bits
            val v = gensym("v");
            val res = gensym("res");
            Result(Var(res),
                    Tup(Decl(U(Var(v))),
                    Tup(Decl(U(Var(res))),
                    Tup(Assign(Var(v), t),
                    Tup(Assign(Var(v), XOR(Var(v), RShift(Var(v), Num(1)))),
                    Tup(Assign(Var(v), XOR(Var(v), RShift(Var(v), Num(2)))),
                    Tup(Assign(Var(v), Times(BitAnd(Var(v), H("0x11111111U")), H("0x1111111111111111UL"))),
                    Tup(Assign(Var(res), BitAnd(RShift(Var(v), Num(28)), Num(1))),
                    Null()))))))))
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

        def ifThenElse[A](b: AST[Boolean])(t: (() => AST[A]))(e: (() => AST[A]))(implicit tag: ClassTag[A]): AST[A] =
        {
            val cond = gensym("cond")
            val conseq = gensym("cons")
            val alt = gensym("alt")
            val conseqAST = classTagToVarNode(tag)(conseq)
            val altAST = classTagToVarNode(tag)(alt)
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
            val conseqAST = classTagToVarNode(tag)(conseq)
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
            val p1 = gensym("p1")
            val p2 = gensym("p2")
            val ret = gensym("str")
            Result(Var(ret),
                Tup(Decl(S(Var(p1))),
                Tup(Decl(S(Var(p2))),
                Tup(Assign(Var(p1), s),
                Tup(Assign(Var(p2), Plus(s, Minus(Length(s), Num(1)))),
                Tup(For(Null(), Gt(Var(p2), Var(p1)), PreInc(Var(p1)),
                        Tup(Assign(Ref(Var(p1)), XOR(Ref(Var(p1)),Ref(Var(p2)))),
                        Tup(Assign(Ref(Var(p2)), XOR(Ref(Var(p2)),Ref(Var(p1)))),
                        Tup(Assign(Ref(Var(p1)), XOR(Ref(Var(p1)),Ref(Var(p2)))),
                        Tup(Assign(Var(p2), Minus(Var(p2), Num(1))), Null()))))), Null()))))))
        }

        def begin[A](as: List[AST[A]]): AST[A] = {
            as.foldRight(Tup(Null(), Null()))({ (e1, e2) => Tup(e1, e2)})
        }

        def prints[A: ClassTag](format: String, es: AST[A]) = {
            val tmpVarName = gensym("tmp")
            val tmp = classTagToVarNode(classTag[A])(tmpVarName)
            val exp = es match {
                case r: Result => r
                case _: Tup | _: Var => Result(Var(tmpVarName),
                                            Tup(Decl(tmp),
                                            Tup(Assign(Var(tmpVarName), es),
                                            Null())))
            }
            Tup(Printf(format, exp), Null())
        }

        def and(a: AST[Boolean], b: AST[Boolean]): AST[Boolean] = And(a, b)
        def or(a: AST[Boolean], b: AST[Boolean]): AST[Boolean] = Or(a, b)

        def lt[A <% Ordered[A]](a: AST[A], b: AST[A]): AST[Boolean] = Lt(a, b)

        def for_(init: AST[Int],
                 cond: AST[Int] => AST[Boolean],
                 variant: AST[Int] => AST[Int],
                 body: AST[Int] => AST[Unit]): AST[Unit] = {
                     var i = gensym("i");
                     Tup(For(AssignInline(I(Var(i)), init),
                             cond(Var(i)),
                             AssignInline(Var(i), variant(Var(i))),
                             body(Var(i))),
                     Null())
                 }
    }

    implicit object EmitParallelAST extends ParallelExp[AST] {
        implicit def d2i(x: Term): Term = x

        def null_() = Null()
        // Wrap EmitTwiddleAST in "omp for" pragma
        def for_(init: Term,
                 cond: Term => Term,
                 variant: Term => Term,
                 body: Term => Term) = Tup(
                                        OmpPragma(
                                                List(("for", Nil)),
                                                EmitTwiddleAST.for_(init, cond, variant, body)
                                            ),
                                        Null())

        def num[A](v: A) = EmitTwiddleAST.num(v)
        def bool(b: Boolean) = EmitTwiddleAST.bool(b)
        def ifThenElse[A](b: AST[Boolean])(t: (() => AST[A]))(e: (() => AST[A]))(implicit tag: ClassTag[A]): AST[A] =
            EmitTwiddleAST.ifThenElse(b)(t)(e)

        def ifThen[A](b: AST[Boolean])(t: (() => AST[A]))(implicit tag: ClassTag[A]): AST[Unit] =
            EmitTwiddleAST.ifThen(b)(t)
        def ternaryIf[A] : AST[_] => (() => AST[_]) => (() => AST[_]) => AST[_] =
                b => t => e => EmitTwiddleAST.ternaryIf(b)(t)(e)

        def add[A: Numeric](a: AST[A], b: AST[A]): AST[A] = EmitTwiddleAST.add(a, b)
        def sub[A: Numeric](a: AST[A], b: AST[A]): AST[A] = EmitTwiddleAST.sub(a, b)
        def mul[A: Numeric](a: AST[A], b: AST[A]): AST[A] = EmitTwiddleAST.mul(a, b)
        def div(a: AST[Int], b: AST[Int]): AST[Int] = EmitTwiddleAST.div(a, b)
        def mod(a: AST[Int], b: AST[Int]): AST[Int] =  EmitTwiddleAST.mod(a, b)
        def prints[A: ClassTag](format: String, es: AST[A]) = EmitTwiddleAST.prints(format, es)

        def and(a: AST[Boolean], b: AST[Boolean]): AST[Boolean] = EmitTwiddleAST.and(a, b)
        def or(a: AST[Boolean], b: AST[Boolean]): AST[Boolean] = EmitTwiddleAST.or(a, b)
        def lt[A <% Ordered[A]](a: AST[A], b: AST[A]): AST[Boolean] = EmitTwiddleAST.lt(a, b)
    }
}