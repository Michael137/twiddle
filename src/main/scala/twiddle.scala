import twiddle.dsl.Interpreter._
import twiddle.dsl.Syntax._

object Main {
    def main(args: Array[String]): Unit = {
        println(example1(Show))
        println(example1(Eval))

        println(example2(Show))
        println(example2(Eval)({ x: Int => x + x })(10))
        // ? println(exampleX(Stage))
        // ? println(exampleX(Optimize))
    }
}