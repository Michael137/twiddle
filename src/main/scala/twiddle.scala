import twiddle.dsl.Interpreter._
import twiddle.dsl.Syntax._
import twiddle.dsl.CodeGen._
import twiddle.dsl.Examples._
import twiddle.lms.dsl.DslDriver
import scala.lms.common._

object Main {

def testDsl() = {
  println(example1(Show))
  println(example1(Eval))

  println(example2(Show))
  println(example2(Eval)({ x: Int => x + x })(10))
  //println(exampleX(Staged))
  // ? println(exampleX(Optimize))

  println(example3(EmitTwiddleAST))
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

def testTwDsl() = {
  import twiddle.dsl.eval._
  def Y(c: Boolean) = L(c, "fun", A(L(c, "F", A(V("F"), List(V("F")))), List(L(c, "F", A(V("fun"), List(L(c, "x", A(A(V("F"), List(V("F"))), List(V("x"))))))))))
  def fib(c: Boolean) = L(c, "fib", L(c, "n", If(A(P("<"), List(V("n"), I(2))), V("n"), A(P("+"), List(A(V("fib"), List(A(P("-"), List(V("n"), I(1))))), A(V("fib"), List(A(P("-"), List(V("n"), I(2))))))))))
  def sumf(c: Boolean) = L(c, "f", L(c, "sumf", L(c, "n", If(A(P("<"), List(V("n"), I(0))), I(0), A(P("+"), List(A(V("f"), List(V("n"))), A(V("sumf"), List(A(P("-"), List(V("n"), I(1)))))))))))

  top_eval[NoRep](A(A(Y(false), List(fib(false))), List(I(7))))
  //top_eval[NoRep](A(A(Y(false), List(fib(true))), List(I(7))))
}

def main(args: Array[String]): Unit = {
    testDsl
    // testLmsDsl
    // testTwDsl()
}

}