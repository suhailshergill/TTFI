package sss.initial

object OOP {
  // extending language is easy, adding more functions is hard since it
  // involves having to modify each case class
  trait Exp[A] {
    def eval: A
  }
  case class Lit(x: Integer) extends Exp[Integer] {
    def eval = x
  }
  case class Neg(x: Exp[Integer]) extends Exp[Integer] {
    def eval = -x.eval
  }
  case class Add(x: Exp[Integer], y: Exp[Integer]) extends Exp[Integer] {
    def eval = x.eval + y.eval
  }
  val tf1 = Add(
    Lit(8),
    Neg(
      Add(
        Lit(1),
        Lit(2)
      )
    )
  )
}
