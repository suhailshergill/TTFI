package sss.finals

object TreeSem {
  // our extensible serialization format
  sealed trait Tree[T]
  case class Leaf[T](data: String) extends Tree[T]
  case class Node[T](data: String, rest: Seq[Tree[T]]) extends Tree[T]

  import Numeric._
  import ExpSym._
  // serializer for ExpSym
  implicit object ExpSym_Tree extends ExpSym[Tree] {
    def lit = (x: Integer) => Node("Lit", Seq(Leaf(s"${x}")))
    def neg = x => Node("Neg", Seq(x))
    def add = x => y => Node("Add", Seq(x, y))
  }
  // run the serializer
  def toTree[T]: Tree[T] => Tree[T] = identity

  import MulSym._
  // extending the serializer for MulSym
  implicit object MulSym_Tree extends MulSym[Tree] {
    def mul = x => y => Node("Mul", Seq(x, y))
  }

  // deserialization
  type ErrMsg = String
  def safeRead(x: String): Either[ErrMsg, Integer] = try {
    Right(x.toInt)
  } catch {
    case e: NumberFormatException => Left(s"Read error in ${x}")
  }

  // {{{ ClosedRecursion

  object ClosedRecursion {
    // this, given a Tree gives ExpSym[repr] => repr[Integer] w/ error msgs
    def fromTree[repr[_]](x: Tree[Integer])(implicit s1: ExpSym[repr]): Either[ErrMsg, repr[Integer]] = x match {
      case Node("Lit", Seq(Leaf(x))) => safeRead(x).right.map(s1.lit(_))
      case Node("Neg", Seq(x)) => fromTree[repr](x).right.map(s1.neg(_))
      case Node("Add", Seq(x, y)) => for {
        a <- fromTree[repr](x).right
        b <- fromTree[repr](y).right
      } yield s1.add(a)(b)
      case _ => Left(s"Parse error in ${x}")
    }

    // first attempt to extend deserializer. does *NOT* work
    def fromTreeExt[repr[_]](x: Tree[Integer])(implicit s1: MulSym[repr], s2: ExpSym[repr]): Either[ErrMsg, repr[Integer]] = x match {
      case Node("Mul", Seq(x, y)) => for {
        a <- fromTreeExt[repr](x).right
        b <- fromTreeExt[repr](y).right
      } yield s1.mul(a)(b)
      case x => fromTree[repr](x)
    }
  }

  // }}}

  object OpenRecursion {
    import sss.predef._, Fix.instances._

    type FromTree[repr[_]] = Tree[Integer] => Fix.T[Either[ErrMsg, repr[Integer]]]
    def fromTree_[repr[_]](s1: ExpSym[repr])(self: FromTree[repr]): FromTree[repr] = x => x match {
      case Node("Lit", Seq(Leaf(x))) => Fix.T.done(safeRead(x).right.map(s1.lit(_)))
      case Node("Neg", Seq(x)) => for {
        y <- Fix.T.suspend(self(x))
      } yield y.right.map(s1.neg(_))
      case Node("Add", Seq(x, y)) => for {
        x <- Fix.T.suspend(self(x))
        y <- Fix.T.suspend(self(y))
      } yield for {
        a <- x.right
        b <- y.right
      } yield s1.add(a)(b)
      case _ => Fix.T.done(Left(s"Parse error in ${x}"))
    }
    def fromTree[repr[_]](x: Tree[Integer])(implicit s1: ExpSym[repr]) = Fix(fromTree_[repr](s1))(x).run

    def fromTreeExt_[repr[_]](s1: (MulSym[repr], ExpSym[repr]))(self: FromTree[repr]): FromTree[repr] = x => x match {
      case Node("Mul", Seq(x, y)) => for {
        x <- Fix.T.suspend(self(x))
        y <- Fix.T.suspend(self(y))
      } yield for {
        a <- x.right
        b <- y.right
      } yield s1._1.mul(a)(b)
      case x => fromTree_(s1._2)(self)(x)
    }
    def fromTreeExt[repr[_]](x: Tree[Integer])(implicit s1: MulSym[repr], s2: ExpSym[repr]): Either[ErrMsg, repr[Integer]] = Fix(fromTreeExt_[repr]((s1, s2)))(x).run

    // {{{ duplicating interpreter

    import ExpSym._
    trait :>[R1[_], R2[_]] {
      type τ[T] = (R1[T], R2[T])
    }
    implicit def ExpSym_Dup[R1[_], R2[_]](implicit s1: ExpSym[R1], s2: ExpSym[R2]): ExpSym[(R1 :> R2)#τ] = new ExpSym[(R1 :> R2)#τ] {
      def lit = (x: Integer) => (s1.lit(x), s2.lit(x))
      def neg = (x: (R1 :> R2)#τ[Integer]) =>
        (s1.neg(x._1), s2.neg(x._2))
      def add = (x1: (R1 :> R2)#τ[Integer]) => (x2: (R1 :> R2)#τ[Integer]) =>
        (s1.add(x1._1)(x2._1), s2.add(x1._2)(x2._2))
    }

    def duplicate[R1[_]: ExpSym, R2[_]: ExpSym, A](x: (R1[A], R2[A])) = x

    def check_consume[A, B](f: A => Unit)(x: Either[ErrMsg, A]) = x match {
      case Left(e) => println(s"Error: ${e}")
      case Right(x) => f(x)
    }

    def dup_consume[R1[_]: ExpSym, R2[_]: ExpSym, A](f: R1[A] => Any) = (x: (R1[A], R2[A])) => {
      val (x1, x2) = duplicate(x)
      println(f(x1))
      x2
    }

    import Exp._
    def thrice =
      dup_consume[Eval, (Debug :> Tree)#τ, Integer](eval) andThen
        dup_consume(view) andThen
        (x => println(toTree(x).toString))

    // }}}
  }
}
