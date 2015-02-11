package finals

object Calculus {

  import Exp._

  trait Symantics[repr[_]] {
    def lit: Int => repr[Int]
    def add: repr[Int] => repr[Int] => repr[Int]
    def lam[a, b]: (repr[a] => repr[b]) => repr[a => b]
    def app[a, b]: repr[a => b] => repr[a] => repr[b]
  }

  implicit object EvalSym extends Symantics[Eval] {
    def lit = Eval(_)
    def add = x => y => Eval(eval(x) + eval(y))
    def lam[a, b] = f => Eval(x => eval(f(Eval(x))))
    def app[a, b] = f => x => Eval(f.value(x.value))
  }

  case class Show[T](f: Int => String) extends Repr[T]
  def show[T]: Show[T] => String = _.f(0)
  implicit object ShowSym extends Symantics[Show] {
    def lit = x => Show(Function.const(x.toString) _)
    def add = e1 => e2 => Show(v => s"(${e1.f(v)} + ${e2.f(v)})")
    def lam[a, b] = e => Show { h =>
      val x = s"x$h"
      s"(\\_$x → ${e(Show(Function.const(x))).f(h + 1)})"
    }
    def app[a, b] = e1 => e2 => Show(h =>
      s"${e1.f(h)}(${e2.f(h)})")
  }

  trait Lifted[repr[_]] {
    def lift[a, b]: (a => b) => repr[a => b]
  }

  implicit object EvalLifted extends Lifted[Eval] {
    def lift[a, b] = f => Eval(f)
  }

  implicit object ShowLifted extends Lifted[Show] {
    def lift[a, b] = f => Show { h =>
      val x = s"x$h"
      s"(\\_$x → /lift/($x))"
    }
  }
}