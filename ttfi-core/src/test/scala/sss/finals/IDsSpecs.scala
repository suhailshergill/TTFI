package sss.finals

import org.specs2._

/**
  * a sans-shapeless solution to the problem described and solved here:
  * <https://github.com/AlecZorab/solid-disco/blob/master/ShapelessIds.ipynb>. note
  * that the solution below uses the tagless final approach to bake in
  * extensibility for multiple interpretations (which adds to the complexity),
  * but it's not strictly needed for the problem at hand.
  *
  * per aleczorab: "if I have multiple different classes with a field called
  * (say) `id` and a `String => T` constructor, how do I make a `(T, String) =>
  * T` whilst minimising the amount of boilerplate needed for each different
  * class. Extra style points for type safety, avoiding "undesirable" behaviour
  * for whatever heuristics one has for undesirable and, as ever, compile time
  * is better than runtime"
  *
  * solution:
  * - id = a method declared in trait ID. this gives `T => String`
  * - `String => T` constructors for the various IDs are implicits
  * - trait `IDSym` embodies the methods of the DSL which when defined once can
  *   be used by all IDs
  */
object IDsImpl {
  import cats._
  import cats.syntax.semigroup._

  /**
    * the trait encapsulating the DSL primitives that we want.
    *
    * note, that it is parameterized on a representation domain as well as a
    * type which defines the universe of IDs. the representation domain (repr)
    * can best be understood as corresponding to a unique semantic
    * interpretation of the DSL (eg. evaluation, debugging etc).
    */
  abstract class IDSym[repr[_]: Functor, id] {
    def lit[T](x: T): repr[T]
    def unwrap[T <: id](id: repr[T]): repr[String]
    def add[T <: id](id: repr[T], str: repr[String])(implicit
      wrap: String => T,
      sg: Semigroup[repr[String]]): repr[T]
  }

  /**
    * the semantic domain corresponding to program evaluation.
    */
  case class Eval[T](value: T)
  // some convenience implicits for working with 'Eval'
  implicit object EvalAsFunctor extends Functor[Eval] {
    def map[A, B](fa: Eval[A])(f: A => B) = Eval(f(fa.value))
  }
  implicit def EvalLiftSemigroup[T](implicit sg: Semigroup[T]): Semigroup[Eval[T]] = new Semigroup[Eval[T]] {
    def combine(x: Eval[T], y: Eval[T]) = Eval(x.value |+| y.value)
  }
  implicit object StringAsSemigroup extends Semigroup[String] {
    def combine(x: String, y: String) = x ++ y
  }

  /**
    * the base trait for all IDs in the system
    */
  trait ID extends Any { self =>
    def id: String
  }
  /**
    * evaluation semantics for IDs in our DSL
    */
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.NonUnitStatements"))
  implicit object IDSym_ID extends IDSym[Eval, ID] {
    val f = implicitly[Functor[Eval]]; import f._

    def lit[T](x) = Eval[T](x)
    def unwrap[T <: ID](id: Eval[T]): Eval[String] = map(id)(_.id)
    def add[T <: ID](id: Eval[T], str: Eval[String])(implicit
      wrap: String => T,
      sg: Semigroup[Eval[String]]): Eval[T] = {
      map(unwrap(id) |+| str)(wrap)
    }
  }

  // some IDs - note the 'implicit' keyword
  implicit class Foo(val id: String) extends AnyVal with ID
  implicit class Bar(val id: String) extends AnyVal with ID
  implicit class Baz(val id: String) extends AnyVal with ID

  object Uses {
    // TODO: generalize the usage here to be extensible to other semantic
    // representations (by parametrizing on `repr[_]`)
    val sym = implicitly[IDSym[Eval, ID]]; import sym._

    /**
      * some syntactic sugar specialized for evaluation semantics.
      */
    implicit class IDEvalOps[T <: ID](me: T) {
      def +(that: String)(implicit wrap: String => T): T = {
        add(lit(me), lit(that)).value
      }
    }

    val t1 = Foo("foo") + "bar"
    val t2 = Bar("bar") + "baz"
    val t3 = Baz("baz") + "qu"
  }
}

class IDsSpecs extends Specification {
  def is = s2"""
  IDSym $id
"""

  import IDsImpl._
  def id = {
    println(Uses.t1)
    println(Uses.t2.id)
    println(Uses.t3.id)
    2 === 2
  }
}
