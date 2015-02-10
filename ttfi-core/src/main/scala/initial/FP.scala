package initial

object FP {
  sealed trait Exp[A]
  case class Lit(x: Integer) extends Exp[Integer]
  case class Neg(x: Exp[Integer]) extends Exp[Integer]
  case class Add(x: Exp[Integer], y: Exp[Integer]) extends Exp[Integer]

  // adding multiple eval functions is easy as long as we don't extend the
  // language
  def eval[A](x: Exp[A]): A = x match {
    case Lit(x) => x
    case Neg(x) => -eval(x)
    case Add(x, y) => eval(x) + eval(y)
  }
  def view[A](x: Exp[A]): String = x match {
    case Lit(x) => x.toString
    case Neg(x) => s"(-${view(x)})"
    case Add(x, y) => s"(${view(x)} + ${view(y)})"
  }

  val tf1 = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

  // pushNeg
  def pushNeg(x: Exp[Integer]): Exp[Integer] = x match {
    case e @ Lit(_) => e
    case e @ Neg(Lit(_)) => e
    // pattern matching being used to decide when to cancel Neg
    case Neg(Neg(e)) => pushNeg(e)
    // see how processing of a negated expression depends on the form of
    // said expression, breaking context insensitivity and leaking
    // abstraction. *not* structurally inductive
    case Neg(Add(e1, e2)) => Add(pushNeg(Neg(e1)), pushNeg(Neg(e2)))
    case Add(e1, e2) => Add(pushNeg(e1), pushNeg(e2))
  }
  val result = view(pushNeg(tf1))
}