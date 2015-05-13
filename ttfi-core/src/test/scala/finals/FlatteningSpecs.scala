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

  val tf3View = view(tf3[Exp.Debug])

  val tf3PushNegView = view(PushNeg(tf3[Ctx_=>[Exp.Debug]#τ]))
  val tf3FlatteningView = view(Flattening(tf3[LCACtx_=>[Exp.Debug]#τ]))

  object Normalize {
    trait Norm[repr[_]] {
      type Flattenable[T] = LCACtx_=>[repr]#τ[T]
      type PushNegAndThenFlatten[T] = Ctx_=>[Flattenable]#τ[T]
      // a representation domain where we have to perform PushNeg followed by
      // Flattening is what we are defining as being "Normalizable"
      type Normalizable[T] = PushNegAndThenFlatten[T]
      type τ[T] = Normalizable[T] // to be consistent with finals.PushNeg etc
    }
    // scala is unable to come up with the below, so let's spoonfeed it some
    implicit def ev[repr[_]: ExpSym]: ExpSym[Norm[repr]#Normalizable] =
      ExpSym_Ctx[Norm[repr]#Flattenable]
    def apply[repr[_]](e: Norm[repr]#Normalizable[Integer]): repr[Integer] = {
      // defining the normalization computation. this is the composition of
      // PushNeg.apply and Flattening.apply. this only works/makes sense when the
      // expression falls under the "Normalizable" representation domain. in
      // such cases, PushNeg.apply transforms it to the "Flattenable"
      // representation domain while Flattening.apply takes care of the actual
      // flattening.
      Flattening(PushNeg[Norm[repr]#Flattenable](e))
    }
  }
  val tf3NormView: String = {
    import Normalize._
    view(Normalize(tf3[Norm[Exp.Debug]#Normalizable]))
  }

  def e1 = tf3View === "((8 + (-(1 + 2))) + (-(-(8 + (-(1 + 2))))))"
  def e2 = tf3PushNegView === "((8 + ((-1) + (-2))) + (8 + ((-1) + (-2))))"
  def e3 = tf3FlatteningView === "(8 + ((-(1 + 2)) + (-(-(8 + (-(1 + 2)))))))"
  def e4 = tf3NormView === "(8 + ((-1) + ((-2) + (8 + ((-1) + (-2))))))"
}
