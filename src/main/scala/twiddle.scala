import twiddle.dsl.Interpreter._
import twiddle.dsl.Syntax._
import twiddle.lms.dsl.DslDriver
import scala.lms.common._

object Main {

def testDsl() = {
  println(example1(Show))
  println(example1(Eval))

  println(example2(Show))
  println(example2(Eval)({ x: Int => x + x })(10))
  // ? println(exampleX(Stage))
  // ? println(exampleX(Optimize))
}

def testLmsDsl() = {
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
    def snippet(x: Rep[Int]) = {

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

  val test_xor = new DslDriver[Int,Unit] {
    def snippet(x: Rep[Int]) = {

      def compute(b: Boolean): Int = {
        val x = 0x55
        val y = x ^ 0x20
        y
      }
      println(compute(x==1))
    }
  }
  println(test_xor.code)

  val test_mod = new DslDriver[Int,Unit] {
    def snippet(x: Rep[Int]) = {

      def compute(b: Rep[Int]): Rep[Int] = {
        val x = 0x55
        val y = (b % 2) ^ (x & b)
        tw_log10(y)
      }
      println(compute(24))
    }
  }
  println(test_mod.code)

  val test_log2 = new DslDriver[Float,Unit] {
    def snippet(x: Rep[Float]) = {
      println(tw_log2(x))
    }
  }
  println(test_log2.code)

  val test_string = new DslDriver[String,Unit] {
    def snippet(x: Rep[String]) = {
      println(tw_reverse(x))
    }
  }
  println(test_string.code)
}

def testArithDsl() = {

}

def main(args: Array[String]): Unit = {
    // testDsl
    testLmsDsl
    // testArithDsl()
}

}