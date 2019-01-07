package twiddle.dsl

import scala.language.higherKinds

import Syntax._
import Interpreter._
import TwiddleAST._
import CodeGen._

object TwiddleTests {
    var testsRun = 0
    def check(a:Any, dst: String) = if (a.toString.trim != dst.trim) {
        println("error: expected ")
        println("    "+dst)
        println("but got")
        println("    "+a)
        (new AssertionError).printStackTrace
    } else testsRun += 1

    def ifTest[T[_]](s:Exp[T]) = {
        import s._
        (ifThenElse(bool(true))
            (() => add(num(3.5), num(3.6)))
            (() => num(5.5)))
    }

    def lamTest[T[_]](s:Exp[T]) = {
        import s._
        lam[Int => Int,Int => Int] (f =>
            lam[Int,Int] (x => 
                app (f) (app(f)(x))))
    }

    def beginTest[T[_]](s:Exp[T]) = {
        import s._
        val if_ret = (ifThenElse(bool(true))
                        (() => log2(num(3)))
                        (() => add(num(2), num(5))))
        val add_ret = add(num(5), num(5))
        val add_log_ret = add(num(5.0), log2(num(3)))
        val add_log_twice_ret = add(log2(num(5)), log2(num(3)))
        cdr(cdr(cdr(cons(if_ret, cons(add_ret, cons(add_log_ret, add_log_twice_ret))))))
    }

    def logTest[T[_]](s:Exp[T]) = {
        import s._
        add(num(5.0), log10(num(10)))
        log10(num(10))
    }

    def stringTest[T[_]](s:Exp[T]) = {
        import s._
        reverse(string("Hello, World!"))
    }

    def printTest[T[_]](s:Exp[T]) = {
        import s._
        val v1 = log10(num(10.2))
        val v2 = log10(num(20.2))
        val v3 = log10(num(30.2))
        begin(List(prints(""""%d %d %d\n"""", List(v1, v2, v3))))
    }

    def lamTest2[T[_]](s:Exp[T]) = {
        import s._
        val lambda = lam[Double,Double] (x => log10(x))
        app(lambda)(num(20))
    }

    def reversebitTest[T[_]](s:Exp[T]) = {
        import s._
        // bits(426)
        List(reverseBits(bits(num(999999900))), reverseBits(bits(num(999009999))))
    }

    def ifTest2[T[_]](s:Exp[T]) = {
        import s._
        (ifThenElse(bool(false))
            (() => add(num(3.5), num(3.6)))
            (() => (ifThenElse(bool(true))
                    (() => add(num(3.5), num(3.6)))
                    (() => num(5.5)))))
    }

    def hasZeroTest[T[_]](s:Exp[T]) = {
        import s._
        hasZero(bits(num(20)))
    }

    def swapBitsTest[T[_]](s:Exp[T]) = {
        import s._
        swapBits(bits(num(20)), bits(num(30)))
    }

    def swapBitsTest2[T[_]](s:Exp[T]) = {
        import s._
        val a = bits(num(20))
        (ifThen(hasZero(a))
                (() => swapBits(a, bits(log10(num(30))))))
    }
}

object Main {
    import TwiddleTests._

    def testPrinter() = {
        check(ifTest(Show), "(if true then (3.5 + 3.6) else 5.5)")
        check(ifTest2(Show), "(if false then (3.5 + 3.6) else (if true then (3.5 + 3.6) else 5.5))")
        check(lamTest(Show), "\\x1.\\x2.(x1 (x1 x2))")
        check(lamTest2(Show), "(\\x3.log10(x3) 20.0)")
        check(beginTest(Show), "(cdr (cdr (cdr (cons (if true then log10(3.0)/log10(2) else (2.0 + 5.0)) (cons (5 + 5) (cons (5.0 + log10(3.0)/log10(2)) (log10(5.0)/log10(2) + log10(3.0)/log10(2))))))))")
        check(logTest(Show), "log10(10.0)")
        check(stringTest(Show), "\"Hello, World!\".reverse")
        check(reversebitTest(Show), "List(0b11100010001100101001101100110001, 0b11100010001110100010100010011000)")
        check(printTest(Show), "(begin , (print , log10(10.2), log10(20.2), log10(30.2)))")
        check(hasZeroTest(Show), "(hasZero? 0b00000000000000000000000000001010)")
        check(swapBitsTest(Show), "(0b00000000000000000000000000001111, 0b00000000000000000000000000001010)")
    }

    def testEval() = {
        check(ifTest(Eval), "7.1")
        check(ifTest2(Eval), "7.1")
        check(lamTest(Eval)({ x: Int => x + x })(10), "40")
        check(lamTest2(Eval), "1.3010299956639813")
        check(beginTest(Eval), "3.9068905956085187")
        check(logTest(Eval), "1.0")
        check(stringTest(Eval), "!dlroW ,olleH")
        check(reversebitTest(Eval), """List(BitSet(1, 5, 6, 9, 10, 12, 13, 16, 18, 21, 22, 26, 30, 31, 32), BitSet(4, 5, 8, 12, 14, 18, 20, 21, 22, 26, 30, 31, 32))""")
        check(printTest(Eval), "()")
        check(hasZeroTest(Eval), "true")
        check(swapBitsTest(Eval), "(BitSet(1, 2, 3, 4),BitSet(2, 4))")
        check(swapBitsTest2(Eval), "(BitSet(0),BitSet(2, 4))")
    }

    def testEmitTwiddleAST() = {
        check(ifTest(EmitTwiddleAST), "Tup(Decl(I(Var(cond1))),Tup(Decl(D(Var(cons2))),Tup(Decl(D(Var(alt3))),Tup(Assign(Var(cond1),Tup(Bool(true),Null())),Tup(IfThenElse(Var(cond1),Assign(Var(cons2),Tup(Plus(Tup(Num(3.5),Null()),Tup(Num(3.6),Null())),Null())),Assign(Var(alt3),Tup(Num(5.5),Null()))),Null())))))")
        eval(lamTest(EmitTwiddleAST)) // TODO: no output currently
        eval(lamTest2(EmitTwiddleAST))
        check(beginTest(EmitTwiddleAST), "Tup(Plus(Result(Var(ret15),Tup(Decl(F(Var(x14))),Tup(Decl(I(Var(ret15))),Tup(Assign(Var(x14),Num(5.0)),Tup(Assign(Var(ret15),Ref(Cast(Const(IntPtr()),Addr(Var(x14))))),Tup(Assign(Var(ret15),Minus(RShift(Var(ret15),Num(23)),Num(127))),Null())))))),Result(Var(ret17),Tup(Decl(F(Var(x16))),Tup(Decl(I(Var(ret17))),Tup(Assign(Var(x16),Num(3.0)),Tup(Assign(Var(ret17),Ref(Cast(Const(IntPtr()),Addr(Var(x16))))),Tup(Assign(Var(ret17),Minus(RShift(Var(ret17),Num(23)),Num(127))),Null()))))))),Null())")
        check(logTest(EmitTwiddleAST), "Result(Var(ret19),Tup(Decl(I(Var(ret19))),Tup(Assign(Var(ret19),TernaryIf(Gte(Num(10.0),Num(1000000000)),Num(9),TernaryIf(Gte(Num(10.0),Num(100000000)),Num(8),TernaryIf(Gte(Num(10.0),Num(10000000)),Num(7),TernaryIf(Gte(Num(10.0),Num(1000000)),Num(6),TernaryIf(Gte(Num(10.0),Num(100000)),Num(5),TernaryIf(Gte(Num(10.0),Num(10000)),Num(4),TernaryIf(Gte(Num(10.0),Num(1000)),Num(3),TernaryIf(Gte(Num(10.0),Num(100)),Num(2),TernaryIf(Gte(Num(10.0),Num(10)),Num(1),Num(0))))))))))),Null())))")
        check(stringTest(EmitTwiddleAST), "Result(Var(str),Tup(Decl(S(Var(p1))),Tup(Decl(S(Var(p2))),Tup(Assign(Var(p1),CStr(Hello, World!)),Tup(Assign(Var(p2),Plus(CStr(Hello, World!),Minus(Length(CStr(Hello, World!)),Num(1)))),Tup(For(Null(),Gt(Var(p2),Var(p1)),PreInc(Var(p1)),Tup(Assign(Ref(Var(p1)),XOR(Ref(Var(p1)),Ref(Var(p2)))),Tup(Assign(Ref(Var(p2)),XOR(Ref(Var(p2)),Ref(Var(p1)))),Tup(Assign(Ref(Var(p1)),XOR(Ref(Var(p1)),Ref(Var(p2)))),Tup(Assign(Var(p2),Minus(Var(p2),Num(1))),Null()))))),Null()))))))")
        check(reversebitTest(EmitTwiddleAST), "List(Result(Var(r20),Tup(Decl(U(Var(v22))),Tup(Decl(U(Var(r20))),Tup(Decl(I(Var(s21))),Tup(Assign(Var(v22),Num(999999900)),Tup(Assign(Var(r20),Num(999999900)),Tup(Assign(Var(s21),Minus(Times(SizeOf(Var(v22)),Macro(CHAR_BIT)),Num(1))),Tup(For(RShiftEq(Var(v22),Num(1)),Var(v22),RShiftEq(Var(v22),Num(1)),Tup(LShiftEq(Var(r20),Num(1)),Tup(BitOrEq(Var(r20),BitAnd(Var(v22),Num(1))),Tup(PostDec(Var(s21)),Null())))),Tup(LShiftEq(Var(r20),Var(s21)),Null()))))))))), Result(Var(r23),Tup(Decl(U(Var(v25))),Tup(Decl(U(Var(r23))),Tup(Decl(I(Var(s24))),Tup(Assign(Var(v25),Num(999009999)),Tup(Assign(Var(r23),Num(999009999)),Tup(Assign(Var(s24),Minus(Times(SizeOf(Var(v25)),Macro(CHAR_BIT)),Num(1))),Tup(For(RShiftEq(Var(v25),Num(1)),Var(v25),RShiftEq(Var(v25),Num(1)),Tup(LShiftEq(Var(r23),Num(1)),Tup(BitOrEq(Var(r23),BitAnd(Var(v25),Num(1))),Tup(PostDec(Var(s24)),Null())))),Tup(LShiftEq(Var(r23),Var(s24)),Null()))))))))))")
        check(printTest(EmitTwiddleAST), "Tup(Tup(Printf(\"%d %d %d\\n\",List(Result(Var(ret26),Tup(Decl(I(Var(ret26))),Tup(Assign(Var(ret26),TernaryIf(Gte(Num(10.2),Num(1000000000)),Num(9),TernaryIf(Gte(Num(10.2),Num(100000000)),Num(8),TernaryIf(Gte(Num(10.2),Num(10000000)),Num(7),TernaryIf(Gte(Num(10.2),Num(1000000)),Num(6),TernaryIf(Gte(Num(10.2),Num(100000)),Num(5),TernaryIf(Gte(Num(10.2),Num(10000)),Num(4),TernaryIf(Gte(Num(10.2),Num(1000)),Num(3),TernaryIf(Gte(Num(10.2),Num(100)),Num(2),TernaryIf(Gte(Num(10.2),Num(10)),Num(1),Num(0))))))))))),Null()))), Result(Var(ret27),Tup(Decl(I(Var(ret27))),Tup(Assign(Var(ret27),TernaryIf(Gte(Num(20.2),Num(1000000000)),Num(9),TernaryIf(Gte(Num(20.2),Num(100000000)),Num(8),TernaryIf(Gte(Num(20.2),Num(10000000)),Num(7),TernaryIf(Gte(Num(20.2),Num(1000000)),Num(6),TernaryIf(Gte(Num(20.2),Num(100000)),Num(5),TernaryIf(Gte(Num(20.2),Num(10000)),Num(4),TernaryIf(Gte(Num(20.2),Num(1000)),Num(3),TernaryIf(Gte(Num(20.2),Num(100)),Num(2),TernaryIf(Gte(Num(20.2),Num(10)),Num(1),Num(0))))))))))),Null()))), Result(Var(ret28),Tup(Decl(I(Var(ret28))),Tup(Assign(Var(ret28),TernaryIf(Gte(Num(30.2),Num(1000000000)),Num(9),TernaryIf(Gte(Num(30.2),Num(100000000)),Num(8),TernaryIf(Gte(Num(30.2),Num(10000000)),Num(7),TernaryIf(Gte(Num(30.2),Num(1000000)),Num(6),TernaryIf(Gte(Num(30.2),Num(100000)),Num(5),TernaryIf(Gte(Num(30.2),Num(10000)),Num(4),TernaryIf(Gte(Num(30.2),Num(1000)),Num(3),TernaryIf(Gte(Num(30.2),Num(100)),Num(2),TernaryIf(Gte(Num(30.2),Num(10)),Num(1),Num(0))))))))))),Null()))))),Null()),Tup(Null(),Null()))")
        check(hasZeroTest(EmitTwiddleAST), "Result(Var(r30),Tup(Decl(U(Var(r30))),Tup(Decl(U(Var(v29))),Tup(Assign(Var(v29),Num(20)),Tup(Define(HAS_ZERO,List(v),(((v) - 0x01010101UL) & ~(v) & 0x80808080UL)),Tup(Assign(Var(r30),Call(HAS_ZERO,List(Var(v29)))),Null()))))))")
        check(swapBitsTest(EmitTwiddleAST), "Result(Var(r33),Tup(Decl(U(Var(r33))),Tup(Decl(U(Var(v31))),Tup(Decl(U(Var(v32))),Tup(Assign(Var(v31),Num(20)),Tup(Assign(Var(v32),Num(30)),Tup(Define(SWAP,List(a, b),(((a) ^= (b)), ((b) ^= (a)), ((a) ^= (b)))),Tup(Assign(Var(r33),Call(SWAP,List(Var(v31), Var(v32)))),Null()))))))))")
        check(swapBitsTest2(EmitTwiddleAST), "Tup(Decl(I(Var(cond36))),Tup(Decl(U(Var(cons37))),Tup(Assign(Var(cond36),Result(Var(r35),Tup(Decl(U(Var(r35))),Tup(Decl(U(Var(v34))),Tup(Assign(Var(v34),Num(20)),Tup(Define(HAS_ZERO,List(v),(((v) - 0x01010101UL) & ~(v) & 0x80808080UL)),Tup(Assign(Var(r35),Call(HAS_ZERO,List(Var(v34)))),Null()))))))),Tup(IfThen(Var(cond36),Assign(Var(cons37),Result(Var(r41),Tup(Decl(U(Var(r41))),Tup(Decl(U(Var(v39))),Tup(Decl(U(Var(v40))),Tup(Assign(Var(v39),Num(20)),Tup(Assign(Var(v40),Result(Var(ret38),Tup(Decl(I(Var(ret38))),Tup(Assign(Var(ret38),TernaryIf(Gte(Num(30.0),Num(1000000000)),Num(9),TernaryIf(Gte(Num(30.0),Num(100000000)),Num(8),TernaryIf(Gte(Num(30.0),Num(10000000)),Num(7),TernaryIf(Gte(Num(30.0),Num(1000000)),Num(6),TernaryIf(Gte(Num(30.0),Num(100000)),Num(5),TernaryIf(Gte(Num(30.0),Num(10000)),Num(4),TernaryIf(Gte(Num(30.0),Num(1000)),Num(3),TernaryIf(Gte(Num(30.0),Num(100)),Num(2),TernaryIf(Gte(Num(30.0),Num(10)),Num(1),Num(0))))))))))),Null())))),Tup(Define(SWAP,List(a, b),(((a) ^= (b)), ((b) ^= (a)), ((a) ^= (b)))),Tup(Assign(Var(r41),Call(SWAP,List(Var(v39), Var(v40)))),Null())))))))))),Null()))))")
        check(beginTest(EmitTwiddleAST).asInstanceOf[Term], "Tup(Plus(Result(Var(ret50),Tup(Decl(F(Var(x49))),Tup(Decl(I(Var(ret50))),Tup(Assign(Var(x49),Num(5.0)),Tup(Assign(Var(ret50),Ref(Cast(Const(IntPtr()),Addr(Var(x49))))),Tup(Assign(Var(ret50),Minus(RShift(Var(ret50),Num(23)),Num(127))),Null())))))),Result(Var(ret52),Tup(Decl(F(Var(x51))),Tup(Decl(I(Var(ret52))),Tup(Assign(Var(x51),Num(3.0)),Tup(Assign(Var(ret52),Ref(Cast(Const(IntPtr()),Addr(Var(x51))))),Tup(Assign(Var(ret52),Minus(RShift(Var(ret52),Num(23)),Num(127))),Null()))))))),Null())")

        // runsrc(gensrc(ifTest2(EmitTwiddleAST))) // TODO: generates incorrect syntax
    }

    def main(args: Array[String]): Unit = {
        testPrinter
        testEval
        testEmitTwiddleAST

        println(s"====> $testsRun assertions tested <====")
    }
}