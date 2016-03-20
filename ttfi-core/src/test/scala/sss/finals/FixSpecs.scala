package sss.finals

import org.specs2._
import matcher._
class FixSpecs extends Specification {
  def is = s2"""
        $fix1 $fix2 $fix3 $fix4 should not blow the stack stack safe ${tag("slow")}
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

  val alot = (3, 11)
  import TreeSem.OpenRecursion._

  def fix1 = {
    fix(acktabs)(alot) should beStackSafe[Int] // << not stack stafe
  }.pendingUntilFixed

  def fix2 = {
    Fix(acktabs)(alot) should beStackSafe[Int] // << not stack stafe
  }.pendingUntilFixed

  def fix3 = {
    Fix2(acktabs)(alot) should beStackSafe[Int] // << not stack stafe
  }.pendingUntilFixed

  def fix4 = {
    // <http://eed3si9n.com/herding-cats/stackless-scala-with-free-monads.html>
    import cats._, std.all._, free.{Free, Trampoline}, Trampoline._

    def tackermann(in: (Int, Int)): Trampoline[Int] = {
      val (m, n) = in
      if (m <= 0) Trampoline.done(n + 1)
      else if (n <= 0) suspend(tackermann((m - 1, 1)))
      else for {
        a <- suspend(tackermann((m, n - 1)))
        b <- suspend(tackermann((m - 1, a)))
      } yield b
    }

    tackermann(alot).run should beStackSafe[Int]
  }
}
