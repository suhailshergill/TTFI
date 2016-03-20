package sss.finals

trait Repr[T]

object Numeric {
  import Exp._

  import language.implicitConversions._

  trait ExpNumSym[num, repr[_]] {
    def lit: num => repr[num]
    def neg: repr[num] => repr[num]
    def add: repr[num] => repr[num] => repr[num]
  }

  trait MulNumSym[num, repr[_]] {
    def mul: repr[num] => repr[num] => repr[num]
  }

  class ExpSymNumericEval[T](implicit e1: Numeric[T]) extends ExpNumSym[T, Eval] {
    import e1.mkNumericOps
    def lit = Eval(_)
    def neg = x => Eval(-x.value)
    def add = x => y => Eval(x.value + y.value)
  }
  implicit def ExpSymNumericEval[T: Numeric]: ExpNumSym[T, Eval] = new ExpSymNumericEval[T]

  class MulSymNumericEval[T](implicit e1: Numeric[T]) extends MulNumSym[T, Eval] {
    import e1.mkNumericOps
    def mul = x => y => Eval(x.value * y.value)
  }
  implicit def MulSymNumericEval[T: Numeric]: MulNumSym[T, Eval] = new MulSymNumericEval[T]
}
