package sss.finals

import org.specs2._
class NumericsSpecs extends Specification {
  def is = s2"""
  num $num
"""

  import Exp._
  import Numeric._

  def num = {
    def tf1[T, repr[_]](implicit s1: ExpNumSym[T, repr], e1: Numeric[T]): repr[T] = {
      import s1._; import e1._
      implicit def fromInt = e1.fromInt _
      add(neg(lit(10)))(lit(10))
    }

    import scala.math.Numeric.LongIsIntegral
    eval(tf1[Long, Eval]) ==== 0
  }
}
