package finals

import org.specs2._
class SimplyTypedLambdaCalculusSpecs extends Specification {
  def is = s2"""
  eval $eval_
  show $show_
"""

  import Exp._
  import Calculus._

  def th1[repr[_]](implicit s: Symantics[repr]) = {
    import s._
    add(lit(1))(lit(2))
  }
  def th2[repr[_]](implicit s: Symantics[repr]) = {
    import s._
    lam((x: repr[Int]) => add(x)(x))
  }
  def th3[repr[_]](implicit s: Symantics[repr]) = {
    import s._
    lam((f: repr[Int => Int]) => add(app(f)(lit(1)))(lit(2)))
  }

  def th4[repr[_]](implicit s: Symantics[repr], l: Lifted[repr]) = {
    import s._; import l._
    lam((f: repr[Int => Int]) => add(app(f)(lit(1)))(lit(2)))
  }

  def eval_ = {
    eval(th1[Eval]) ==== 3 &&
      eval(th4[Eval]) ==== 2
  }

  def show_ = {
    show(th1[Show]) ==== "(1 + 2)" &&
      show(th2[Show]) ==== "(\\_x0 → (x0 + x0))" &&
      show(th3[Show]) ==== "(\\_x0 → (x0(1) + 2))"
  }
}