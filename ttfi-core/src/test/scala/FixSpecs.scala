import TTFI._

import org.specs2._
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

  def stacksafe[T](v: => T) = {
    import scala.util.{ Try, Failure, Success }
    Try(v) match {
      case _: Success[T] => ok
      case Failure(_: StackOverflowError) => ko("stack overflow")
      case Failure(e) => ko(s"unexpected failure $e")
    }
  }

  val alot = (3, 11)

  import Final.TreeSem.OpenRecursion._

  def fix1 = {
    // stacksafe(fix(acktabs)(alot)) // << not stack stafe
    pending
  }

  def fix2 = {
    // stacksafe(Fix(acktabs)(alot)) // << not stack stafe
    pending
  }

  def fix3 = {
    // stacksafe(Fix2(acktabs)(alot)) // << not stack stafe
    pending
  }

  def fix4 = {
    // https://github.com/scalaz/scalaz/blob/series/7.2.x/example/src/main/scala/scalaz/example/TrampolineUsage.scala
    import scalaz._, Scalaz._, Free._
    import scalaz.std._

    def tackermann(in: (Int, Int)): Trampoline[Int] = {
      val (m, n) = in
      if (m <= 0) return_(n + 1)
      else if (n <= 0) suspend(tackermann((m - 1, 1)))
      else for {
        a <- suspend(tackermann((m, n - 1)))
        b <- suspend(tackermann((m - 1, a)))
      } yield b
    }

    stacksafe(tackermann(alot).run)
  }
}