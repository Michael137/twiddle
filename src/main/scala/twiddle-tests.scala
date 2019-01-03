package twiddle.dsl

import scala.language.higherKinds

import Syntax._
import Interpreter._
import TwiddleAST._
import CodeGen._

object Examples {
    def example1[T[_]](s:CExp[T]) = {
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

    def example3[T[_]](s:CExp[T]) = {
        import s._
        // ! todo should generate code for both statements
        val if_ret = (ifThenElse_(bool(true))
                        (() => log2(num(3)))
                        (() => add(num(2), num(5))))
        val add_ret = add(num(5), num(5))
        val add_log_ret = add(num(5), log2(num(3)))
        val add_log_twice_ret = add(log2(num(5)), log2(num(3)))
        cdr(cdr(cdr(cons(if_ret, cons(add_ret, cons(add_log_ret, add_log_twice_ret))))))
        // `(,if_ret ,add_ret ,add_log_ret)
    }

    def example4[T[_]](s:CExp[T]) = {
        import s._
        add(num(5), log10(num(10)))
        log10(num(10))
    }

    def example5[T[_]](s:CExp[T]) = {
        import s._
        reverse(string("Hello, World!"))
    }

    // def example6[T[_]](s:CExp[T]) = {
    //     import s._
    //     val v1 = (() => log10(num(1)))
    //     val v2 = (() => log10(num(2)))
    //     val v3 = (() => log10(num(3)))
    //     begin(v1, v2, v3)
    // }

    // def example7[T[_], M[_]](s1:CExp[T], s2:EmitC[M]) = {
    //     import s._
    // }
}

object Main {
    import Examples._

    def testDsl() = {
        println(example1(Show))
        println(example1(Eval))

        println(example2(Show))
        println(example2(Eval)({ x: Int => x + x })(10))
        //println(exampleX(Staged))
        // ? println(exampleX(Optimize))

        println(example3(Show))
        // println(example3(Eval)) // TODO: non-terminating
        println(example3(EmitTwiddleAST))
        eval(example3(EmitTwiddleAST))

        println(example4(Show))
        // println(example4(Eval)) // TODO: non-terminating
        println(example4(EmitTwiddleAST))
        eval(example4(EmitTwiddleAST))

        println(example5(Show))
        println(example5(Eval))
        println(example5(EmitTwiddleAST))
        eval(example5(EmitTwiddleAST))
    }

    def main(args: Array[String]): Unit = {
        testDsl
    }
}