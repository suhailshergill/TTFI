package sss.finals

object Exp {
  // container to hold the result of 'Eval'-ing an expression
  case class Eval[T](value: T) extends Repr[T]
  // run the 'Eval' interpreter
  def eval[T]: Eval[T] => T = _.value

  // container to hold the result of 'pretty-printing' an expression
  case class Debug[T](debug: String) extends Repr[T]
  // run the pretty-printing interpreter
  def view[T]: Debug[T] => String = _.debug
}
