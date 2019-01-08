package twiddle.dsl

import scala.language.higherKinds
import Syntax._
import Interpreter._
import TwiddleAST._
import CodeGen._

object Playground {
    def example[T[_]](s: Exp[T]) {
        import s._
        // Write your code here

        // Check ``src/main/scala/core.scala'' or ``twiddle.pdf''
        // for supported features
    }    
}

object Main {
    import TwiddleTests._

    def main(args: Array[String]): Unit = {}
}