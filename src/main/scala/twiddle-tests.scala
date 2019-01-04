package twiddle.dsl

import scala.language.higherKinds

import Syntax._
import Interpreter._
import TwiddleAST._
import CodeGen._

object Examples {
    def example1[T[_]](s:Exp[T]) = {
        import s._
        // the term is
        //    if true then 3 + 4
        //    else 5
        (ifThenElse_(bool(true))
            (() => add(num(3.5), num(3.6)))
            (() => num(5.5)))
    }

    def example2[T[_]](s:Exp[T]) = {
        import s._
        lam[Int => Int,Int => Int] (f =>
            lam[Int,Int] (x => 
                app (f) (app(f)(x))))
    }

    def example3[T[_]](s:Exp[T]) = {
        import s._
        // ! todo should generate code for both statements
        val if_ret = (ifThenElse_(bool(true))
                        (() => log2(num(3)))
                        (() => add(num(2), num(5))))
        val add_ret = add(num(5), num(5))
        val add_log_ret = add(num(5.0), log2(num(3)))
        val add_log_twice_ret = add(log2(num(5)), log2(num(3)))
        cdr(cdr(cdr(cons(if_ret, cons(add_ret, cons(add_log_ret, add_log_twice_ret))))))
        begin(List(prints(""""%d\n"""", List(log2(num(55.5))))))
        // `(,if_ret ,add_ret ,add_log_ret)
    }

    def example4[T[_]](s:Exp[T]) = {
        import s._
        add(num(5.0), log10(num(10)))
        log10(num(10))
    }

    def example5[T[_]](s:Exp[T]) = {
        import s._
        reverse(string("Hello, World!"))
    }

    def example6[T[_]](s:Exp[T]) = {
        import s._
        val v1 = log10(num(10.2))
        val v2 = log10(num(20.2))
        val v3 = log10(num(30.2))
        begin(List(prints(""""%d %d %d\n"""", List(v1, v2, v3))))
    }

    def example7[T[_]](s:Exp[T]) = {
        import s._
        val lambda = lam[Double,Double] (x => log10(x))
        app(lambda)(num(20))
    }

    def example8[T[_]](s:Exp[T]) = {
        import s._
        // bits(426)
        prints(""""%d"""", List(reverseBits(bits(100))))
    }
}

object Main {
    import Examples._

    def testDsl() = {
        println(example1(Show))
        println(example1(Eval))
        println(example1(EmitTwiddleAST))
        eval(example1(EmitTwiddleAST))
 
        println(example2(Show))
        println(example2(Eval)({ x: Int => x + x })(10))
        println(example2(EmitTwiddleAST))

        println(example3(Show))
        println(example3(Eval))
        println(example3(EmitTwiddleAST))
        eval(example3(EmitTwiddleAST))
        runsrc(gensrc(example3(EmitTwiddleAST)))

        println(example4(Show))
        println(example4(Eval))
        println(example4(EmitTwiddleAST))
        eval(example4(EmitTwiddleAST))
 
        println(example5(Show))
        println(example5(Eval))
        println(example5(EmitTwiddleAST))
        eval(example5(EmitTwiddleAST))

        println(example6(Show))
        println(example6(Eval))
        println(example6(EmitTwiddleAST))
        eval(example6(EmitTwiddleAST))
        runsrc(gensrc(example6(EmitTwiddleAST)))
        
        println(example7(Show))
        println(example7(Eval))
        println(example7(EmitTwiddleAST))
        eval(example7(EmitTwiddleAST))
        runsrc(gensrc(example7(EmitTwiddleAST)))

        println(example8(Show))
        println(example8(Eval))
        println(example8(EmitTwiddleAST))
        eval(example8(EmitTwiddleAST))
        runsrc(gensrc(example8(EmitTwiddleAST)))
    }

    def main(args: Array[String]): Unit = {
        testDsl
    }
}