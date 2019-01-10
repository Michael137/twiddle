package twiddle.dsl

import scala.language.higherKinds
import Syntax._
import Interpreter._
import AstInterpreter._
import TwiddleAST._
import Codegen._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object InterpreterMode extends Enumeration {
  type ModeType = Value
  val Twiddle = Value("twiddle")
  val Eval = Value("eval")
  val Print = Value("print")
  val Parallel = Value("parallel")

  def isModeType(s: String) = values.exists(_.toString == s)
}

object REPL {
    private var mode = InterpreterMode.Twiddle
    private var shouldGenerate = false
    def switchMode(m: String) = {
        try {
            val newMode = InterpreterMode.withName(m);
            mode = newMode;
            println(s"==> Mode switched to ${mode.toString}")
        } catch {
            case e: Exception => println(s"==> Failed to switch to unknown mode ${m}")
        }
    }
    def getInterpObjectString(m: InterpreterMode.Value) = m match {
        case InterpreterMode.Twiddle => "twiddle.dsl.AstInterpreter.EmitTwiddleAST"
        case InterpreterMode.Parallel => "twiddle.dsl.AstInterpreter.EmitParallelAST"
        case InterpreterMode.Eval => "twiddle.dsl.Interpreter.Eval"
        case InterpreterMode.Print => "twiddle.dsl.Interpreter.Show"
    }

    def main(args: Array[String]): Unit = {
        val modes = for (v <- InterpreterMode.values) yield "'" + v.toString + "'"
        println(s"#### Starting Twiddle REPL in ${mode.toString} mode ####")
        println(s"""#### Other options are: ${modes.mkString(",")} ####""")
        println(s"#### Toggle on code generation mode on/off with 'c'/'!c'")
        println("#### Press 'q' to quit")
        println
        Iterator.continually({print(">>> ");
                              io.StdIn.readLine})
                .takeWhile(_ != "q")
                .foreach {
                    case s if(InterpreterMode.isModeType(s)) => switchMode(s)
                    case "c" => shouldGenerate = true; println(s"==> Code generation mode *ON*")
                    case "!c" => shouldGenerate = false; println(s"==> Code generation mode *OFF*")
                    case input => {
                        def snippet(s: String) = {
                            import EmitTwiddleAST._
                            val toolbox = currentMirror.mkToolBox()
                            val interp = getInterpObjectString(mode)
                            val interpWrapper = s"""
                                import twiddle.dsl._;
                                import $interp._;
                                import twiddle.dsl.Syntax._;
                                def exec[T[_]] = (s: Exp[T]) => {
                                    import s._
                                    $input
                                }
                                (exec)($interp)
                            """
                            val res = toolbox.eval(toolbox.parse(interpWrapper))
                            if(shouldGenerate) Codegen.eval(res.asInstanceOf[GeneralAST.ASTNode]) else res
                        }

                        try {
                            snippet(input) match {
                                case Unit => ()
                                case v => println(v)
                            }
                        } catch { case e: Exception => println(s"Execution engine failed to run code:\n\n\t$e\nTry switching off code generation mode.") }
                    }
                }
    }
}