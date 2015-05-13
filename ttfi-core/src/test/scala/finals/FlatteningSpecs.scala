package finals

import org.specs2.Specification

class FlatteningSpecs extends Specification {
  def is = s2"""
  ti3 view $e1
  ti3 push neg view $e2
  ti3 flattening view $e3
  ti3 normalized view $e4
"""

  import Exp.view
  import ExpSym.ExpSym
  import ExpSym.ExpSym_Debug

  def tf1[repr[_]](implicit s: ExpSym[repr]): repr[Integer] = {
    import s._
    add(
      lit(8))(
        neg(
          add(
            lit(1))(
              lit(2))))
  }

  def tf3[repr[_]](implicit s: ExpSym[repr]): repr[Integer] = {
    import s._
    add(tf1(s))(neg(neg(tf1(s))))
  }

  import PushNeg._
  import Flattening._

  type PushNegCtx[T] = PushNeg.Ctx_=>[Exp.Debug]#τ[T]
  type FlatCtx[T] = Flattening.LCACtx_=>[Exp.Debug]#τ[T]

  val tf3View = view(tf3[Exp.Debug])

  val tf3PushNegView = view(PushNeg(tf3[Ctx_=>[Exp.Debug]#τ]))
  val tf3FlatteningView = view(Flattening(tf3[LCACtx_=>[Exp.Debug]#τ]))

  def e1 = tf3View === "((8 + (-(1 + 2))) + (-(-(8 + (-(1 + 2))))))"
  def e2 = tf3PushNegView === "((8 + ((-1) + (-2))) + (8 + ((-1) + (-2))))"
  def e3 = tf3FlatteningView === "(8 + ((-(1 + 2)) + (-(-(8 + (-(1 + 2)))))))"
  def e4 = pending // tf3NormView === "(8 + ((-1) + ((-2) + (8 + ((-1) + (-2))))))"
}
