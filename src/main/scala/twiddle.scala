import twiddle.dsl.Interpreter._
import twiddle.dsl.Syntax._
import twiddle.lms.dsl.DslDriver
import scala.lms.common._

object Main {
    def main(args: Array[String]): Unit = {
        println(example1(Show))
        println(example1(Eval))

        println(example2(Show))
        println(example2(Eval)({ x: Int => x + x })(10))
        // ? println(exampleX(Stage))
        // ? println(exampleX(Optimize))

        val snippet = new DslDriver[Int,Int] {
          def snippet(x: Rep[Int]) = {

            def compute(b: Rep[Boolean]): Rep[Int] = {
              // the if is executed in the first stage
              if (b) 1 else x
            }

            // compute(x==1)
            compute(1==1)
          }
        }
        println(snippet.code)

      val range_snippet = new DslDriver[Int,Unit] {
        def snippet(x: Rep[Int]) = comment("for", verbose = false) {

          for (i <- (0 until x): Rep[Range]) {
            println(i)
          }

        }
      }
      println(range_snippet.code)

      val test_snipet = new DslDriver[Int,Unit] {
        def snippet(x: Rep[Int]) = {

          def compute(b: Boolean): String = {
            // the if is executed in the first stage
            if (b) "Hello" else "World!"
          }

          var y = compute(x==1)
          println(y)
        }
      }
      println(test_snipet.code)
    }
}