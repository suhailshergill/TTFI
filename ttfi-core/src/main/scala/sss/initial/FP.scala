package sss.initial

object FP {
  sealed trait Exp[A]
  case class Lit(x: Integer) extends Exp[Integer]
  case class Neg(x: Exp[Integer]) extends Exp[Integer]
  case class Add(x: Exp[Integer], y: Exp[Integer]) extends Exp[Integer]

  // adding multiple eval functions is easy as long as we don't extend the
  // language
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.AsInstanceOf"))
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

  val ti1 = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

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

  val ti1Norm = pushNeg(ti1)
  val ti1NormView = view(ti1Norm)
  val ti1NormEval = eval(ti1Norm)

  // flata
  def flata(x: Exp[Integer]): Exp[Integer] = x match {
    case e @ Lit(_) => e
    case e @ Neg(_) => e
    case Add(Add(e1, e2), e3) => flata(Add(e1, Add(e2, e3)))
    case Add(e1, e2) => Add(e1, flata(e2))
  }

  val norm = flata _ compose pushNeg _
  val ti3 = Add(ti1, Neg(Neg(ti1)))
  val ti3View = view(ti3)
  val ti3Norm = norm(ti3)
  val ti3NormView = view(ti3Norm)
}
