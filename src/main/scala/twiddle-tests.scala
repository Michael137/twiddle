package twiddle.dsl

import twiddle.dsl.Interpreter._
import twiddle.dsl.Syntax._
import twiddle.dsl.CodeGen._
import twiddle.dsl.Examples._
import scala.language.higherKinds

object Examples {
    import Syntax._
    import CodeGen._
    import Interpreter._
    def example1[T[_]](s:Exp[T]) : T[Double] = {
        import s._
        // the term is
        //    if true then 3 + 4
        //    else 5
        (ifThenElse_(bool(true))
            (() => add(num(3.5), num(3.6)))
            (() => num(5.5)))
    }

    def example2[T[_]](s:Exp[T]) : T[(Int => Int) => (Int => Int)] = {
        import s._
        lam[Int => Int,Int => Int] (f =>
            lam[Int,Int] (x => 
                app (f) (app(f)(x))))
    }

    def example3[T[_]](s:CExp[T]) : T[Int] = {
        import s._
        (ifThenElse_(bool(true))
            (() => log2(num(3)))
            (() => add(num(2), num(5))))
    }
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

        println(example3(EmitTwiddleAST))
        eval(example3(EmitTwiddleAST))
    }

    def main(args: Array[String]): Unit = {
        testDsl
    }
}