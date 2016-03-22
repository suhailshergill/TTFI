package sss.utils

import org.specs2._
import matcher._
class FixSpecs extends Specification {
  def is = s2"""
  Fixpoint operator should not blow the stack       ${section("slow")}
  Fix.apply $fix
  Fix.Exception $fixException
  Fix.fixSimple $fixSimple                          ${section("slow")}
"""

  // en.wikipedia.org/wiki/Ackermann_function
  def acktabs(ack: ((Int, Int)) => Int)(in: (Int, Int)): Int = {
    val (m, n) = in
    if (m <= 0) n + 1
    else if (n <= 0) ack((m - 1, 1))
    else ack((m - 1, ack((m, n - 1))))
  }

  def beStackSafe[T]: Matcher[T] =
    throwA[StackOverflowError].not.setMessage("This code is not stacksafe")

  val alot = (3, 12)

  def fixException = {
    Fix.Exception(acktabs)(alot) should beStackSafe[Int] // << not stack stafe
  }.pendingUntilFixed
  // no point checking below till above is fixed
  def fixException2 = {
    def notLeakException[T]: Matcher[T] =
      throwA[Fix.Exception.FixException].not.setMessage("This should not happen")

    Fix.Exception(acktabs)(alot) should notLeakException[Int]
  }

  def fixSimple = {
    Fix.fixSimple(acktabs)(alot) should beStackSafe[Int]
  }.pendingUntilFixed

  def fix = {
    import Fix.instances._

    // <http://eed3si9n.com/herding-cats/stackless-scala-with-free-monads.html>
    def acktabs(ack: ((Int, Int)) => Fix.T[Int])(in: (Int, Int)): Fix.T[Int] = {
      val (m, n) = in
      if (m <= 0) Fix.T.done(n + 1)
      else if (n <= 0) Fix.T.suspend(ack((m - 1, 1)))
      else for {
        a <- Fix.T.suspend(ack((m, n - 1)))
        b <- Fix.T.suspend(ack((m - 1, a)))
      } yield b
    }
    Fix(acktabs)(alot).run should beStackSafe[Int]
  }
}
