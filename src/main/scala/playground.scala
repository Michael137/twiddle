package twiddle.dsl

import scala.language.higherKinds
import Syntax._
import Interpreter._
import AstInterpreter._
import TwiddleAST._
import Codegen._

object Playground {
    def example[T[_]](s: Exp[T]) = {
        import s._
        // Write your code here

        // Check ``src/main/scala/core.scala'' or ``twiddle.pdf''
        // for supported features
    }

    def anotherExample[T[_]](s: Exp[T]) = {
        import s._
        begin(
            List(
                prints(quote(raw"%s\n"), string("This is an example string")),
                for_(num(50), i => gte(i, num(0)), i => sub(i, num(1)),
                    i => prints(quote("%d "), mod(mul(i, num(20)), num(15)))),
                prints(quote(raw"\nBits: %d"), bits(num(256)))
            )
        )
    }

    def main(args: Array[String]): Unit = {
        /*
        ** Evaluate
        */
        anotherExample(Eval)

        /*
        ** Pretty-print
        */
        val stringified = anotherExample(Show)
        println(stringified)

        /*
        ** Twiddle
        */
        val twiddled = anotherExample(EmitTwiddleAST)
        println(twiddled) // Print the IR
        eval(twiddled) // Print twiddled C
        val filename = gensrc(twiddled) // Generate twiddle.c source file
        runsrc(filename) // compile and run emitted C

        /*
        ** Parallel
        */
        val parallel = anotherExample(EmitParallelAST)
        println(parallel)
        eval(parallel) // Print OpenMP annotated C
    }
}