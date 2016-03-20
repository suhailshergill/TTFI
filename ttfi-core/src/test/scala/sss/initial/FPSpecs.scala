package sss.initial

import org.specs2._
import matcher._

class FPSpecs extends Specification {
  def is = s2"""
  pushNeg
    ti1 normalized expression $e1
    ti1 normalized view $e2
    ti1 normalized eval $e3

  norm = flata o pushNeg
    ti3 view $e4
    ti3 push neg view $e5
    ti3 flata view $e6
    ti3 normalized $e7
    ti3 normalized view $e8
"""

  import FP._

  def e1 = ti1Norm === Add(Lit(8), Add(Neg(Lit(1)), Neg(Lit(2))))
  def e2 = ti1NormView === "(8 + ((-1) + (-2)))"
  def e3 = ti1NormEval must_=== 5

  def e4 = ti3View === "((8 + (-(1 + 2))) + (-(-(8 + (-(1 + 2))))))"
  def e5 = view(pushNeg(ti3)) === "((8 + ((-1) + (-2))) + (8 + ((-1) + (-2))))"
  def e6 = view(flata(ti3)) === "(8 + ((-(1 + 2)) + (-(-(8 + (-(1 + 2)))))))"
  def e7 = ti3Norm ===
    Add(
      Lit(8),
      Add(
        Neg(Lit(1)),
        Add(
          Neg(Lit(2)),
          Add(
            Lit(8),
            Add(
              Neg(Lit(1)),
              Neg(Lit(2))
            )
          )
        )
      )
    )
  def e8 = ti3NormView === "(8 + ((-1) + ((-2) + (8 + ((-1) + (-2))))))"
}
