package sss.utils

object Fix {
  import scala.annotation.tailrec

  // make these available for importers
  val T = cats.free.Trampoline
  type T[A] = cats.free.Trampoline[A]
  val instances = cats.std.all

  // invoke import instances._ when invoking 'run'
  final def apply[A, B](f: (A => T[B]) => (A => T[B])): A => T[B] = {
    f((x: A) => Fix(f)(x))
  }

  // not @tailrec
  def fixSimple[A, B](f: (A => B) => (A => B)): A => B = {
    f((x: A) => fixSimple(f)(x))
  }

  // tail-recursive version of fixpoint, by doubling composition.
  // NOTE: this doesn't work if 'f' is curried. i.e., 'B' cannot be of the
  // form 'C => ...'. the reason is that in that case FixException escapes
  // the context. if 'B' is not a concrete value (but is a function) then
  // 'FixException' is only thrown when it gets applied to something, at
  // which point it's too late (unless we override the .apply function, by
  // creating a 'Fix' object?)
  object Exception {
    case class FixException() extends RuntimeException
    @tailrec final def apply[A, B](f: (A => B) => (A => B))(x: A): B = try {
      f(_ => throw FixException())(x)
    } catch {
      case e: FixException => Exception(f andThen f)(x)
    }
  }

  // TODO: try other variants from
  // <http://okmij.org/ftp/Computation/fixed-point-combinators.html#Self->
}
