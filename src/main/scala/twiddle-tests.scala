package twiddle.dsl

import scala.language.higherKinds

import Syntax._
import Interpreter._
import AstInterpreter._
import TwiddleAST._
import Codegen._

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
        begin(List(prints(""""%d\n"""", v1)))
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

    def parityTest[T[_]](s:Exp[T]) = {
        import s._
        // Parity(20) => 1
        // Parity(19) => 0
        begin(List(bitParity(bits(num(20))), bitParity(bits(num(19)))))
    }

    def forTest[T[_]](s:Exp[T]) = {
        import s._
        for_(num(0), i => lt(i, num(2)), i => add(i, num(1)),
            i => prints[Int](""""%d"""", i))
    }

    def modTest[T[_]](s:Exp[T]) = {
        import s._
        mod(num(12341), num(63))
    }

    def modTest2[T[_]](s:Exp[T]) = {
        import s._
        begin(List(
                mod(num(25), num(5)), // Standard mod
                mod(num(25), num(4)), // Power of 2
                mod(num(25), num(3)) // (Power of 2) - 1
              ))
    }

    def parforTest[T[_]](s:Exp[T]) = {
        import s._
        val x = num(0)
        begin(List(
            for_(x, i => lt(i, num(2)), i => add(i, num(1)),
                i => prints(""""%d"""", add(i, i)))))
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
        check(reversebitTest(Show), "List(0b11000100011001010011011001100011, 0b11000100011101000101000100110000)")
        check(printTest(Show), """(begin , (print "%d\n", log10(10.2)))""")
        check(hasZeroTest(Show), "(hasZero? 0b00000000000000000000000000010100)")
        check(swapBitsTest(Show), "(0b00000000000000000000000000011110, 0b00000000000000000000000000010100)")
        check(forTest(Show).split('\n').map(_.trim.filter(_ >= ' ')).mkString, """for(int i = 0, x < 2, (x + 1)) {(print "%d", x)}""")
        check(parityTest(Show), "(begin , (bits-set-even? 0b00000000000000000000000000010100), (bits-set-even? 0b00000000000000000000000000010011))")
        check(modTest(Show), "(12341 % 63)")
        check(modTest2(Show), "(begin , (25 % 5), (25 % 4), (25 % 3))")
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
        check(forTest(Eval), "()")
        check(parityTest(Eval), "0")
        check(modTest(Eval), "56")
        check(modTest2(Eval), "1")
    }

    def testEmitTwiddleAST() = {
        ifTest(EmitTwiddleAST)
        eval(lamTest(EmitTwiddleAST)) // TODO: no output currently
        eval(lamTest2(EmitTwiddleAST))
        beginTest(EmitTwiddleAST)
        logTest(EmitTwiddleAST)
        stringTest(EmitTwiddleAST)
        reversebitTest(EmitTwiddleAST)
        printTest(EmitTwiddleAST)
        hasZeroTest(EmitTwiddleAST)
        swapBitsTest(EmitTwiddleAST)
        swapBitsTest2(EmitTwiddleAST)
        beginTest(EmitTwiddleAST).asInstanceOf[Term]
        forTest(EmitTwiddleAST).asInstanceOf[Term]
        parityTest(EmitTwiddleAST)
        ifTest2(EmitTwiddleAST)
    }

    def testEmitParallelAST() = {
        gensrc(logTest(EmitParallelAST))
        parforTest(EmitParallelAST)
        swapBitsTest(EmitParallelAST)
        parityTest(EmitParallelAST)
    }

    def main(args: Array[String]): Unit = {
        testPrinter
        testEval
        testEmitTwiddleAST
        testEmitParallelAST

        println(s"====> $testsRun assertions tested <====")
    }
}