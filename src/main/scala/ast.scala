package twiddle.dsl

object GeneralAST {
    import ParallelAST._
    import TwiddleAST._

    sealed trait ASTNode
}

import GeneralAST._

object TwiddleAST {
    type AST[A] = Term
    abstract class Term extends ASTNode
    case class IntPtr() extends Term
    case class Const(e: Term) extends Term
    case class Addr(e: Term) extends Term
    case class Var(s: String) extends Term
    case class Num[A](n: A) extends Term
    case class Bool(b: Boolean) extends Term
    case class CStr(s: String) extends Term // C-style string
    case class Gte(e1: Term, e2: Term) extends Term
    case class Gt(e1: Term, e2: Term) extends Term
    case class Lt(e1: Term, e2: Term) extends Term
    case class PreInc(e1: Term) extends Term
    case class Assign(v: Term, e: Term) extends Term
    case class AssignInline(v: Term, e: Term) extends Term
    case class Decl(v: Term) extends Term
    case class I(v: Var) extends Term // Integer
    case class H(s: String) extends Term // Hex number
    case class B(s: String) extends Term // Bit number
    case class F(v: Var) extends Term // Float
    case class D(v: Var) extends Term // Double
    case class S(v: Var) extends Term // String
    case class U(v: Var) extends Term // Unsigned Integer (also used to represent bits)
    case class RShift(v: Term, n: Term) extends Term // >>
    case class LShift(v: Term, n: Term) extends Term // <<
    case class Minus(v: Term, n: Term) extends Term // -
    case class Plus(v: Term, n: Term) extends Term // +
    case class Times(v: Term, n: Term) extends Term // *
    case class Divide(v: Term, n: Term) extends Term // /
    case class Mod(v: Term, n: Term) extends Term // /
    case class XOR(n1: Term, n2: Term) extends Term // ^
    case class Cast(c: Term, e: Term) extends Term
    case class IfThenElse(cond: Term, conseq: Term, alt: Term) extends Term
    case class IfThen(cond: Term, conseq: Term) extends Term
    case class TernaryIf(cond: Term, conseq: Term, alt: Term) extends Term
    case class Ref(e: Term) extends Term // pointer dereference
    case class Null() extends Term // null type
    case class Length(s: CStr) extends Term
    case class For(init: Term, cond: Term, variant: Term, body: Term) extends Term
    case class Printf(format: String, vars: Term) extends Term // TODO: vars should be List[Term]
    case class SizeOf(v: Term) extends Term
    case class Macro(s: String) extends Term
    case class RShiftEq(a: Term, b: Term) extends Term
    case class LShiftEq(a: Term, b: Term) extends Term
    case class BitOrEq(a: Term, b: Term) extends Term
    case class BitOr(a: Term, b: Term) extends Term
    case class BitAnd(a: Term, b: Term) extends Term
    case class Or(a: Term, b: Term) extends Term
    case class And(a: Term, b: Term) extends Term
    case class PostDec(a: Term) extends Term
    case class Func(ret: String, name: String, params: List[String], body: Term => Term) extends Term
    case class App(f: Term, arg: Term) extends Term // f can be Var or Func
    case class Define(vr: String, args: List[String], vl: String) extends Term // TODO: Could be improved by taking Term instead of simple String
    case class Call(f: String, arg: List[Var]) extends Term // TODO: f should be Var

    case class Tup(e1: Any, e2: Any) extends Term // tuple // ! should be Tup(e1: Term, e2: Term)

    case class Program(name: String, e: Term) extends Term
    case class Result(v: Var, e: Term) extends Term

    var varCtr = 0
    def fresh() = { varCtr += 1; varCtr }
    def gensym(varName: String) = { s"$varName${fresh()}" }
    def reset() = { varCtr = 0 }
}

object ParallelAST {
    import TwiddleAST._
    type PAST[T] = PTerm
    abstract class PTerm extends ASTNode

     // #pragma omp name1(params1..n1) name2(params2...n2) ... nameN(paramsN...nN) { /* body */ }
    case class OmpPragma(pragmas: List[(String, List[String])], body: Term) extends PTerm
}