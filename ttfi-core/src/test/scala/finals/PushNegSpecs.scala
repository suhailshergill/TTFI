package finals

import org.specs2._
class PushNegSpecs extends Specification {
  def is = s2"""
  show $show
  eval_ $eval_
  same $same
"""

  import Exp._
  import ExpSym._
  import MulSym._
  import PushNeg._

  def tf1[repr[_]](implicit s: ExpSym[repr]): repr[Integer] = {
    import s._
    add(
      lit(8))(
        neg(
          add(
            lit(1))(
              lit(2))))
  }

  def tfm1[repr[_]](implicit e: ExpSym[repr], m: MulSym[repr]) = {
    import e._
    import m._

    add(
      lit(8))(
        neg(
          mul(
            lit(1))(
              lit(2))))
  }

  val vtf1 = PushNeg(tf1[Ctx_=>[Eval]#τ]).value
  val vtfm1 = PushNeg(tfm1[Ctx_=>[Eval]#τ]).value

  def show = {
    view(PushNeg(tf1[Ctx_=>[Debug]#τ])) ==== "(8 + ((-1) + (-2)))" &&
      view(PushNeg(tfm1[Ctx_=>[Debug]#τ])) ==== "(8 + (1 * (-2)))"
  }

  def eval_ = {
    vtf1 ==== 5 &&
      vtfm1 ==== 6
  }

  def same = {
    eval(tf1[Eval]) ==== vtf1 &&
      eval(tfm1[Eval]) ==== vtfm1
  }
}
