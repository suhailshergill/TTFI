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
      sealed trait Exp[A]
      case class Lit(x: Integer) extends Exp[Integer]
      case class Neg(x: Exp[Integer]) extends Exp[Integer]
      case class Add(x: Exp[Integer], y: Exp[Integer]) extends Exp[Integer]

      // adding multiple eval functions is easy as long as we don't extend the
      // language
      def eval[A](x: Exp[A]): A = x match {
        case Lit(x) => x
        case Neg(x) => -eval(x)
        case Add(x, y) => eval(x) + eval(y)
      }
      def view[A](x: Exp[A]): String = x match {
        case Lit(x) => x.toString
        case Neg(x) => s"(-${view(x)})"
        case Add(x, y) => s"(${view(x)} + ${view(y)})"
      }

      val tf1 = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

      // pushNeg
      def pushNeg(x: Exp[Integer]): Exp[Integer] = x match {
        case e @ Lit(_) => e
        case e @ Neg(Lit(_)) => e
        // pattern matching being used to decide when to cancel Neg
        case Neg(Neg(e)) => pushNeg(e)
        // see how processing of a negated expression depends on the form of
        // said expression, breaking context insensitivity and leaking
        // abstraction. *not* structurally inductive
        case Neg(Add(e1, e2)) => Add(pushNeg(Neg(e1)), pushNeg(Neg(e2)))
        case Add(e1, e2) => Add(pushNeg(e1), pushNeg(e2))
      }
      val result = view(pushNeg(tf1))
    }

    // }}}
  }

  object Final {
    // {{{ ExpSym

    object ExpSym {
      trait ExpSym[repr] {
        def lit: Integer => (Integer, repr)
        def neg: ((Integer, repr)) => (Integer, repr)
        def add: ((Integer, repr)) => ((Integer, repr)) => (Integer, repr)
      }

      implicit object ExpSym_Integer extends ExpSym[Integer] {
        def lit = (x: Integer) => (null, x)
        def neg = (x: (Integer, Integer)) => (x._1, -x._2)
        def add = (x: (Integer, Integer)) => (y: (Integer, Integer)) => (y._1, x._2 + y._2)
      }
      // run the Integer interpreter
      def eval: ((Any, Integer)) => Integer = _._2

      implicit object ExpSym_String extends ExpSym[String] {
        def lit = (x: Integer) => (null, x.toString)
        def neg = (x: (Integer, String)) => (x._1, s"(-${x._2})")
        def add = (x: (Integer, String)) => (y: (Integer, String)) => (y._1, s"(${x._2} + ${y._2})")
      }
      // run the String interpreter
      def view: ((Any, String)) => String = _._2

      object Use {
        def tf1[repr](implicit s1: ExpSym[repr]): (Integer, repr) = {
          s1.add(s1.lit(8))(s1.neg(s1.add(s1.lit(1))(s1.lit(2))))
        }
        def tf2[T](implicit s1: ExpSym[T]): (Integer, T) = {
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
      // add multiplication operation to the Exp dsl
      trait MulSym[repr] {
        def mul: ((Integer, repr)) => ((Integer, repr)) => (Integer, repr)
      }

      // multiplication for Integer domain
      implicit object MulSym_Integer extends MulSym[Integer] {
        def mul = (x: (Integer, Integer)) => (y: (Integer, Integer)) => (y._1, x._2 * y._2)
      }
      implicit object MulSym_String extends MulSym[String] {
        def mul = (x: (Integer, String)) => (y: (Integer, String)) => (y._1, s"(${x._2} * ${y._2})")
      }

      object Use {
        import ExpSym._
        def tfm1[repr](implicit s1: ExpSym[repr], s2: MulSym[repr]) = {
          s1.add(s1.lit(8))(s1.neg(s2.mul(s1.lit(1))(s1.lit(2))))
        }

        val result = eval(tfm1[Integer])
        val result2 = view(tfm1[String])
      }
    }

    // }}}

    // {{{ PushNeg

    object PushNeg {
      // make the context on which the operation depends explicit
      sealed trait Ctx
      case object Pos extends Ctx
      case object Neg extends Ctx

      // pass Ctx to a Ctx dependent expression
      def passCtx[T](x: Ctx)(e: (Integer, Ctx => T)): (Integer, T) = (e._1, e._2(x))
      // PushNeg.apply === pushNeg (in Initial version). pass in the 'no-op'/
      // base context
      def apply[T](e: (Integer, Ctx => T)): (Integer, T) = passCtx(Pos)(e)

      import ExpSym._
      // what we'd like is something like the following:
      // implicit object ExpSym_Ctx[R](implicit s1: ExpSym[R]) extends ExpSym[Ctx => R]
      //
      // due to limitation that scala objects need to have a concrete type, this
      // needs to be an 'implicit class'. _x is needed due to the requirement that
      // implicit classes have one argument
      implicit class ExpSym_Ctx[R](_x: Any = null)(implicit s1: ExpSym[R]) extends ExpSym[Ctx => R] {
        def lit = (x: Integer) => (null, (ctx: Ctx) => (ctx match {
          case Pos => s1.lit(x)
          case Neg => s1.neg(s1.lit(x))
        })._2)
        def neg = (e: (Integer, Ctx => R)) => (e._1, (ctx: Ctx) => ctx match {
          case Pos => e._2(Neg)
          case Neg => e._2(Pos)
        })
        def add = (e1: (Integer, Ctx => R)) => (e2: (Integer, Ctx => R)) => (e2._1, (ctx: Ctx) =>
          s1.add(passCtx(ctx)(e1))(passCtx(ctx)(e2))._2)
      }

      import MulSym._
      implicit class MulSym_Ctx[R](_x: Any = null)(implicit s1: MulSym[R]) extends MulSym[Ctx => R] {
        def mul = (e1: (Integer, Ctx => R)) => (e2: (Integer, Ctx => R)) => (e2._1, (ctx: Ctx) => (ctx match {
          case Pos => s1.mul(passCtx(Pos)(e1))(passCtx(Pos)(e2))
          case Neg => s1.mul(passCtx(Pos)(e1))(passCtx(Neg)(e2))
        })._2)
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
      // our extensible serialization format
      sealed trait Tree
      case class Leaf(data: String) extends Tree
      case class Node(data: String, rest: Seq[Tree]) extends Tree

      import ExpSym._
      // serializer for ExpSym
      implicit object ExpSym_Tree extends ExpSym[Tree] {
        def lit = (x: Integer) => (null, Node("Lit", Seq(Leaf(s"${x}"))))
        def neg = (x: (Integer, Tree)) => (x._1, Node("Neg", Seq(x._2)))
        def add = (x: (Integer, Tree)) => (y: (Integer, Tree)) => (y._1, Node("Add", Seq(x._2, y._2)))
      }
      // run the serializer
      def toTree: ((Any, Tree)) => Tree = _._2

      val tf1_tree = toTree(ExpSym.Use.tf1[Tree])

      import MulSym._
      // extending the serializer for MulSym
      implicit object MulSym_Tree extends MulSym[Tree] {
        def mul = (x: (Integer, Tree)) => (y: (Integer, Tree)) => (y._1, Node("Mul", Seq(x._2, y._2)))
      }
      val tfm1_tree = toTree(MulSym.Use.tfm1[Tree])

      // deserialization
      type ErrMsg = String
      def safeRead(x: String): Either[ErrMsg, Integer] = try {
        Right(x.toInt)
      } catch {
        case e: NumberFormatException => Left(s"Read error in ${x}")
      }

      // {{{ closed recursion: fromTree, fromTreeExt

      object ClosedRecursion {
        // this, given a Tree gives ExpSym[repr] => (Integer, repr) w/ error msgs
        def fromTree[repr](x: Tree)(implicit s1: ExpSym[repr]): Either[ErrMsg, (Integer, repr)] = x match {
          case Node("Lit", Seq(Leaf(x))) => safeRead(x).right.map(s1.lit(_))
          case Node("Neg", Seq(x)) => fromTree[repr](x).right.map(s1.neg(_))
          case Node("Add", Seq(x, y)) => for {
            a <- fromTree[repr](x).right
            b <- fromTree[repr](y).right
          } yield s1.add(a)(b)
          case _ => Left(s"Parse error in ${x}")
        }

        def fromTreeExt[T](x: Tree)(implicit s1: MulSym[T], s2: ExpSym[T]): Either[ErrMsg, (Integer, T)] = x match {
          case Node("Mul", Seq(x, y)) => for {
            a <- fromTree[T](x).right
            b <- fromTree[T](y).right
          } yield s1.mul(a)(b)
          case x => fromTree[T](x)
        }
      }

      // }}}

      // {{{ open recursion: fromTree, fromTreeExt

      object OpenRecursion {
        import scala.annotation.tailrec

        // not tail-recursive
        def fix[A, B](f: (A => B) => (A => B)): A => B = {
          f((x: A) => fix(f)(x))
        }

        // tail-recursive version of fixpoint, by doubling composition
        object Fix {
          case class FixException extends RuntimeException
          @tailrec def apply[A, B](f: (A => B) => (A => B))(x: A): B = try {
            f(_ => throw FixException())(x)
          } catch {
            case e: FixException => Fix(f andThen f)(x)
          }
        }

        def fromTree_[repr](s1: ExpSym[repr])(self: Tree => Either[ErrMsg, (Integer, repr)])(x: Tree) = x match {
          case Node("Lit", Seq(Leaf(x))) => safeRead(x).right.map(s1.lit(_))
          case Node("Neg", Seq(x)) => self(x).right.map(s1.neg(_))
          case Node("Add", Seq(x, y)) => for {
            a <- self(x).right
            b <- self(y).right
          } yield s1.add(a)(b)
          case _ => Left(s"Parse error in ${x}")
        }
        def fromTree[T](x: Tree)(implicit s1: ExpSym[T]) = Fix(fromTree_[T](s1))(x)

        def fromTreeExt_[T](s1: (MulSym[T], ExpSym[T]))(self: Tree => Either[ErrMsg, (Integer, T)])(x: Tree): Either[ErrMsg, (Integer, T)] = x match {
          case Node("Mul", Seq(x, y)) => for {
            a <- self(x).right
            b <- self(y).right
          } yield s1._1.mul(a)(b)
          case x => fromTree_(s1._2)(self)(x)
        }
        def fromTreeExt[T](x: Tree)(implicit s1: MulSym[T], s2: ExpSym[T]): Either[ErrMsg, (Integer, T)] = Fix(fromTreeExt_[T]((s1, s2)))(x)
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
          def lit = (x: Integer) => massageTuple(s1.lit(x), s2.lit(x))
          def neg = (x: (Integer, (R1, R2))) => massageTuple(s1.neg((x._1, x._2._1)), s2.neg((x._1, x._2._2)))
          def add = (x: (Integer, (R1, R2))) => (y: (Integer, (R1, R2))) =>
            massageTuple(s1.add((x._1, x._2._1))((y._1, y._2._1)), s2.add((x._1, x._2._2))((y._1, y._2._2)))
        }
        def massageTuple[T, R1, R2](x: (T, R1), y: (T, R2)) = (y._1, (x._2, y._2))

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