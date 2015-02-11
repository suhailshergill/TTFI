package finals

object ExpSym {
  import Exp._

  // let's declare our 'Exp' "datatype", i.e., define syntax
  trait ExpSym[repr[_]] {
    def lit: Integer => repr[Integer]
    def neg: repr[Integer] => repr[Integer]
    def add: repr[Integer] => repr[Integer] => repr[Integer]
  }

  // 'Eval' interpreter definition
  implicit object ExpSym_Eval extends ExpSym[Eval] {
    def lit = Eval(_)
    def neg = x => Eval(-x.value)
    def add = x => y => Eval(x.value + y.value)
  }

  // definition of pretty-printing interpreter
  implicit object ExpSym_Debug extends ExpSym[Debug] {
    def lit = x => Debug(x.toString)
    def neg = x => Debug(s"(-${x.debug})")
    def add = x => y => Debug(s"(${x.debug} + ${y.debug})")
  }
}

object MulSym {
  import Exp._
  import ExpSym._
  // add multiplication operation to the Exp dsl
  trait MulSym[repr[_]] {
    def mul: repr[Integer] => repr[Integer] => repr[Integer]
  }

  // multiplication for Integer domain
  implicit object MulSym_Eval extends MulSym[Eval] {
    def mul = x => y => Eval(x.value * y.value)
  }
  implicit object MulSym_Debug extends MulSym[Debug] {
    def mul = x => y => Debug(s"(${x.debug} * ${y.debug})")
  }
}