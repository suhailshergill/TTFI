import TTFI._

import org.specs2._
class Fix_Spec extends Specification {
  def is = s2"""
	Fix should not blow the stack $stackSafe
"""

  def stackSafe = {

    // en.wikipedia.org/wiki/Ackermann_function
    def acktabs(ack: ((Int, Int)) => Int)(in: ((Int, Int))): Int = {
      val (m, n) = in
      if (m <= 0) n + 1
      else if (n <= 0) ack((m - 1, 1))
      else ack((m - 1, ack((m, n - 1))))
    }

    Final.TreeSem.OpenRecursion.fix(acktabs)((5, 5)) ==== 0
  }
}