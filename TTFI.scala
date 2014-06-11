object TTFI {

  object Initial {
    // {{{ OOP

    object OOP {
      // extending language is easy, adding more functions is hard since it
      // involves having to modify each case class
      trait Exp[A] {
        def eval: A
      }
      case class Lit(x: Integer) extends Exp[Integer] {
        def eval = x
      }
      case class Neg(x: Exp[Integer]) extends Exp[Integer] {
        def eval = -x.eval
      }
      case class Add(x: Exp[Integer], y: Exp[Integer]) extends Exp[Integer] {
        def eval = x.eval + y.eval
      }
      val tf1 = Add(
        Lit(8),
        Neg(
          Add(
            Lit(1),
            Lit(2))))
    }

    // }}}

    // {{{ FP

    object FP {
      // adding multiple eval functions is easy, but extending the language to
      // include different cases is hard
      def eval[A](x: Exp[A]): A = x match {
        case Lit(x) => x
        case Neg(x) => -eval(x)
        case Add(x, y) => eval(x) + eval(y)
      }
      sealed trait Exp[A]
      case class Lit(x: Integer) extends Exp[Integer]
      case class Neg(x: Exp[Integer]) extends Exp[Integer]
      case class Add(x: Exp[Integer], y: Exp[Integer]) extends Exp[Integer]

      val tf1 = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

      // pushNeg
      def pushNeg(x: Exp[Integer]): Exp[Integer] = x match {
        case e @ Lit(_) => e
        case e @ Neg(Lit(_)) => e
        case Neg(Neg(e)) => pushNeg(e)
        case Neg(Add(e1, e2)) => Add(pushNeg(Neg(e1)), pushNeg(Neg(e2)))
        case Add(e1, e2) => Add(pushNeg(e1), pushNeg(e2))
      }
    }

    // }}}
  }

  object Final {
    // {{{ ExpSym

    object ExpSym {
      trait ExpSym[repr] {
        def lit: Integer => repr
        def neg: repr => repr
        def add: repr => repr => repr
      }

      implicit object ExpSym_Integer extends ExpSym[Integer] {
        def lit = (x: Integer) => x
        def neg = (x: Integer) => -x
        def add = (x: Integer) => (y: Integer) => x + y
      }
      def eval: Integer => Integer = identity[Integer] _

      implicit object ExpSym_String extends ExpSym[String] {
        def lit = (x: Integer) => x.toString
        def neg = (x: String) => s"(-${x})"
        def add = (x: String) => (y: String) => s"(${x} + ${y})"
      }
      def view: String => String = identity[String] _

      object Use {
        def tf1[repr](implicit s1: ExpSym[repr]): repr = {
          s1.add(s1.lit(8))(s1.neg(s1.add(s1.lit(1))(s1.lit(2))))
        }
        def tf2[T](implicit s1: ExpSym[T]): T = {
          s1.neg(tf1[T])
        }

        // TODO: obviate need to pass type explicitly
        // in haskell, the type of 'eval' tells the compiler which type dictionary
        // to look at. it can do that because overlapping instances aren't allowed
        // by default. in scala, however, you can have multiple overlapping
        // implicits. the dispatch mechanism then is based on
        // <http://stackoverflow.com/questions/5598085/where-does-scala-look-for-implicits>
        // what we need is a way to thread a type to underlying argument

        val result = eval(tf1[Integer])
        val result2 = view(tf1[String])
      }
    }

    // }}}

    // {{{ MulSym

    object MulSym {
      trait MulSym[repr] {
        def mul: repr => repr => repr
      }

      implicit object MulSym_Integer extends MulSym[Integer] {
        def mul = (x: Integer) => (y: Integer) => x * y
      }
      implicit object MulSym_String extends MulSym[String] {
        def mul = (x: String) => (y: String) => s"(${x} * ${y})"
      }

      object Use {
        import ExpSym._
        def tfm1[repr](implicit s1: ExpSym[repr], s2: MulSym[repr]) = {
          s1.add(s1.lit(8))(s1.neg(s2.mul(s1.lit(1))(s1.lit(2))))
        }

        val result = tfm1[Integer]
        val result2 = tfm1[String]
      }
    }

    // }}}

    // {{{ PushNeg

    object PushNeg {
      sealed trait Ctx
      case object Pos extends Ctx
      case object Neg extends Ctx

      def apply[T](e: Ctx => T): T = e(Pos)

      import ExpSym._
      // due to limitation that scala objects need to have a concrete type, this
      // needs to be an 'implicit class'. _x is needed due to the requirement that
      // implicit classes have one argument
      implicit class ExpSym_Ctx[R](_x: Any = null)(implicit s1: ExpSym[R]) extends ExpSym[Ctx => R] {
        def lit = (x: Integer) => (ctx: Ctx) => ctx match {
          case Pos => s1.lit(x)
          case Neg => s1.neg(s1.lit(x))
        }
        def neg = (e: Ctx => R) => (ctx: Ctx) => ctx match {
          case Pos => e(Neg)
          case Neg => e(Pos)
        }
        def add = (e1: Ctx => R) => (e2: Ctx => R) => (ctx: Ctx) =>
          s1.add(e1(ctx))(e2(ctx))
      }

      import MulSym._
      implicit class MulSym_Ctx[R](_x: Any = null)(implicit s1: MulSym[R]) extends MulSym[Ctx => R] {
        def mul = (e1: Ctx => R) => (e2: Ctx => R) => (ctx: Ctx) => ctx match {
          case Pos => s1.mul(e1(Pos))(e2(Pos))
          case Neg => s1.mul(e1(Pos))(e2(Neg))
        }
      }

      object Use {
        import ExpSym.view
        import ExpSym.Use.tf1
        val result = view(PushNeg(tf1[Ctx => String]({})))

        import MulSym.Use.tfm1
        val result2 = view(PushNeg(tfm1[Ctx => String]({}, {})))
      }
    }

    // }}}

    // {{{ TreeSem

    object TreeSem {
      sealed trait Tree
      case class Leaf(data: String) extends Tree
      case class Node(data: String, rest: Seq[Tree]) extends Tree

      import ExpSym._
      implicit object ExpSym_Tree extends ExpSym[Tree] {
        def lit = (x: Integer) => Node("Lit", Seq(Leaf(s"${x}")))
        def neg = (x: Tree) => Node("Neg", Seq(x))
        def add = (x: Tree) => (y: Tree) => Node("Add", Seq(x, y))
      }
      def toTree: Tree => Tree = identity[Tree] _

      val tf1_tree = toTree(ExpSym.Use.tf1[Tree])

      import MulSym._
      implicit object MulSym_Tree extends MulSym[Tree] {
        def mul = (x: Tree) => (y: Tree) => Node("Mul", Seq(x, y))
      }
      val tfm1_tree = toTree(MulSym.Use.tfm1[Tree])

      // deserialization
      type ErrMsg = String
      import scala.util.control.Exception.catching
      def safeRead(x: String): Either[ErrMsg, Integer] = {
        catching(classOf[Exception]).
          opt(x.toInt) match {
            case None => Left(s"Read error in ${x}")
            case Some(y) => Right(y)
          }
      }

      // {{{ closed recursion: fromTree, fromTreeExt

      object ClosedRecursion {
        // this, given a Tree gives ExpSym[repr] => repr
        def fromTree[repr](x: Tree)(implicit s1: ExpSym[repr]): Either[ErrMsg, repr] = x match {
          case Node("Lit", Seq(Leaf(x))) => safeRead(x).fold(Left(_), x => Right(s1.lit(x)))
          case Node("Neg", Seq(x)) => fromTree[repr](x).fold(Left(_), x => Right(s1.neg(x)))
          case Node("Add", Seq(x, y)) => fromTree[repr](x) match {
            case Right(a) => fromTree[repr](y) match {
              case Right(b) => Right(s1.add(a)(b))
              case x => x
            }
            case x => x
          }
          case _ => Left(s"Parse error in ${x}")
        }

        def fromTreeExt[T](x: Tree)(implicit s1: MulSym[T], s2: ExpSym[T]): Either[ErrMsg, T] = x match {
          case Node("Mul", Seq(x, y)) => fromTree[T](x) match {
            case Right(a) => fromTree[T](y) match {
              case Right(b) => Right(s1.mul(a)(b))
              case x => x
            }
            case x => x
          }
          case x => fromTree[T](x)
        }
      }

      // }}}

      // {{{ open recursion: fromTree, fromTreeExt

      object OpenRecursion {
        import scala.annotation.tailrec

        def fix[A, B](f: (A => B) => (A => B)): A => B = {
          f((x: A) => fix(f)(x))
        }

        object Fix {
          case class FixException extends RuntimeException
          @tailrec def apply[A, B](f: (A => B) => (A => B))(x: A): B = try {
            f(_ => throw FixException())(x)
          } catch {
            case e: FixException => Fix(f andThen f)(x)
          }
        }

        def fromTree_[repr](s1: ExpSym[repr])(self: Tree => Either[ErrMsg, repr])(x: Tree) = x match {
          case Node("Lit", Seq(Leaf(x))) => safeRead(x).fold(Left(_), x => Right(s1.lit(x)))
          case Node("Neg", Seq(x)) => self(x).fold(Left(_), x => Right(s1.neg(x)))
          case Node("Add", Seq(x, y)) => self(x) match {
            case Right(a) => self(y) match {
              case Right(b) => Right(s1.add(a)(b))
              case x => x
            }
            case x => x
          }
          case _ => Left(s"Parse error in ${x}")
        }
        def fromTree[T](x: Tree)(implicit s1: ExpSym[T]) = Fix(fromTree_[T](s1))(x)

        def fromTreeExt_[T](s: (MulSym[T], ExpSym[T]))(self: Tree => Either[ErrMsg, T])(x: Tree): Either[ErrMsg, T] = x match {
          case Node("Mul", Seq(x, y)) => self(x) match {
            case Right(a) => self(y) match {
              case Right(b) => Right(s._1.mul(a)(b))
              case x => x
            }
            case x => x
          }
          case x => fromTree_(s._2)(self)(x)
        }
        def fromTreeExt[T](x: Tree)(implicit s1: MulSym[T], s2: ExpSym[T]): Either[ErrMsg, T] = Fix(fromTreeExt_[T]((s1, s2)))(x)
      }

      // }}}

      object Use {
        // deserialization problem is the problem that we're unable to have a truly
        // generic representation (while allowing for language extensibility). the
        // symptom is having to invoke 'fromTree' twice for two uses. if the
        // language were fixed, what we could do is to create a wrapper
        // representation incorporating all the relevant typeclasses (language
        // constructs). what the duplicating interpretor does is to have these
        // trampoline-like constructs which lazily (per use) invoke, in essence, the
        // fromTree translation (via the ExpSym instance for the 'repr' pair)
        val result = ClosedRecursion.fromTree[String](tf1_tree)
        val result2 = OpenRecursion.fromTree[String](tf1_tree)

        val result3 = ClosedRecursion.fromTreeExt[String](tfm1_tree)
        val result4 = OpenRecursion.fromTreeExt[String](tfm1_tree)

        // {{{ TODO: duplicating interpreter

        import ExpSym._
        // this opens up the door to nested pairs. so we basically have a type-level
        // list-like structure corresponding to the various sequence of operations
        // that we would like to do. then you can call fromTree[(A, (B, (C, D)))]
        // which in one go (and in haskell, lazily) does the requisite mapping to A,
        // B, C, and D types
        implicit class ExpSym_Dup[R1, R2](val x: (R1, R2))(implicit s1: ExpSym[R1], s2: ExpSym[R2]) extends ExpSym[(R1, R2)] {
          def lit = (x: Integer) => (s1.lit(x), s2.lit(x))
          def neg = (x: (R1, R2)) => (s1.neg(x._1), s2.neg(x._2))
          def add = (x: (R1, R2)) => (y: (R1, R2)) => (s1.add(x._1)(y._1), s2.add(x._2)(y._2))
        }

        def check_consume[A, B](f: A => B)(x: Either[ErrMsg, A]) = x match {
          case Left(e) => println(s"Error: ${e}")
          case Right(x) => f(x)
        }
        def dup_consume[A, B](f: A => Any)(x: ExpSym_Dup[A, B]): B = {
          println(f(x.x._1))
          x.x._2
        }

        // }}}
      }
    }

    // }}}
  }

}
