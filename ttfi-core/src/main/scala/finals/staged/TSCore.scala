package finals.staged

/**
 * scala port of <http://okmij.org/ftp/tagless-final/TaglessStaged/TSCore.hs>
 */
object TSCore {
  import scalaz._
  import Scalaz._

  // The object language (EDSL)

  // A Scala value of the type SSym[repr] => repr[a]
  // represents an expression in the object language of the type 'a'

  // There is no lam form; See the trait LamPure below
  // dedicated to abstractions.
  trait SSym[repr[_]] {
    def intS: Int => repr[Int]
    def addS: repr[Int => Int => Int]
    def mulS: repr[Int => Int => Int]

    def appS[a, b]: repr[a => b] => (repr[a] => repr[b])
  }

  def $[repr[_]: SSym, a, b](x: repr[a])(f: repr[a => b]): repr[b] = {
    implicitly[SSym[repr]].appS(f)(x)
  }

  //////////////////////////////////
  // The interpreters for our DSL //
  //////////////////////////////////

  // The evaluator
  case class R[a](unR: a) extends AnyVal
  // In other words:
  // R: a => R[a]
  // unR: R[a] => a
  // which shows the isomorphism between Scala types and DSL types

  // The interpreter
  implicit object SSym_R extends SSym[R] {
    def intS = R.apply
    def addS = R(x => y => x + y)
    def mulS = R(x => y => x * y)
    def appS[a, b] = rf => rx => R(rf.unR apply rx.unR)
  }

  // The C interpreter, the compiler
  // We mostly use it to show the code
  import scala.reflect.runtime.{ universe => u }

  type VarCounter = Int // we will see the need for it shortly, lamS

  trait C[a] {
    def unC: VarCounter => u.Expr[a]
  }
  object C {
    import u._
    // some utils to aid generation of Expr
    import scala.reflect.runtime.currentMirror
    import scala.reflect.api.{ Mirror, TreeCreator, Universe }
    // similar to
    // https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/staging.scala#L87
    // below lacks annotation for TypeTag, iiuc that's more for
    // shapeless. perhaps not since that allows us to get the name of the type
    // and other things at runtime, as per <http://stackoverflow.com/a/31191219>
    def mkExpr[T](tree: Tree): Expr[T] =
      Expr[T](currentMirror, new TreeCreator {
        def apply[U <: Universe with Singleton](m: Mirror[U]): U#Tree =
          if (m eq currentMirror) tree.asInstanceOf[U#Tree]
          else throw new IllegalArgumentException(s"Expr defined in $currentMirror cannot be migrated to other mirrors.")
      })
  }
  def runCS[a]: C[a] => String = cs => cs.unC(0).tree.toString

  // Adding `purely generated' lambda-expressions to our EDSL
  // Any effect of generating their body can't propagate past
  // the binder.
  // See below for more general lambda-expressions
  trait LamPure[repr[_]] {
    def lamS[a: u.TypeTag, b]: (repr[a] => repr[b]) => repr[a => b]
  }

  implicit object Lampure_R extends LamPure[R] {
    def lamS[a: u.TypeTag, b] = rf => R { x => (rf apply R(x)).unR }
  }

  /*
  The connection to scala macros

  Unlike scala macros, based on reification and splicing for building
  code values, we use combinators for code generation (cf. Xi and Thiemann).
  Here is how our combinators could be defined in terms of
  reification and splicing:

  intS x  =  reify(x)
  addS    =  reify((x: Int) => (y: Int) => x + y)
  appS x y = reify(x.splice apply y.splice)

  // lamS, using scala macros directly, would be a cross-stage evaluation as
  // below
  lamS f   =  reify(x => (f apply reify(x)).splice)

  The combinators intS, addS, appS build code _values_, which are inert
  at present stage and whose building involves no effect.
  */
  import Function.const
  implicit object SSym_C extends SSym[C] {
    def intS = x => new C[Int] {
      def unC = const(u reify x)
    }
    def addS = new C[Int => Int => Int] {
      def unC = const(u reify {
        (x: Int) => (y: Int) => x + y
      })
    }
    def mulS = new C[Int => Int => Int] {
      def unC = const(u reify {
        (x: Int) => (y: Int) => x * y
      })
    }
    def appS[a, b] = cf => cx => new C[b] {
      def unC = vc => u reify {
        cf.unC(vc).splice apply cx.unC(vc).splice
      }
    }
  }

  // {{{ scratch

  object Scratch {
    import u._
    type Code[T] = Expr[T]

    // so i can create a term using this
    // combining below with a VarCounter we get same functionality as we would
    // using TermName . Context.freshName
    def term: TermName = TermName("hmm") // term.toString = "hmm"
    def foo: Expr[TermName] = reify { TermName("hmm") }

    def eta[a, b]: (Code[a] => Code[b]) => Code[a => b] = f => {
      //reify((x: a) => f(reify(x)).splice)

      // <https://issues.scala-lang.org/browse/SI-5917> might help?
      ???
    }

    def field: Expr[String] = ???
    //val s: String = c.eval(c.Expr[String](field.tree.duplicate))
    val name: String = "x12"
    val name2: Expr[Int] = C.mkExpr[Int](q"${TermName(name)}")

    //import shapeless.examples.ReflectionUtils.mkExpr
    def expr = C.mkExpr[Int => Int](q"(${TermName(name)}: Int) => ${TermName(name)}")
    def expr2 = {
      C.mkExpr[Int => Int](q"(${TermName(name)}: Int) => ${name2}")
    }

    def add = ((x: Int) => (y: Int) => x + y)
    def addR: u.Expr[Int => Int => Int] = u reify add
    def showExpr[a]: u.Expr[a] => String = e => u showRaw e.tree
    def hmm = u.showRaw(u.reify((x: Int) => (y: Int) => x + y))
    def qu: u.Expr[Int] = u reify { add(1)(2) }
    def qux: u.Tree = qu.tree // 5
    def qux2: String = u showRaw qux // Literal(Constant(5))

    object Context {
      import scala.language.experimental.macros
      import scala.reflect.macros.blackbox.Context
      def hmm_impl(c: Context): c.Expr[Int] = {
        import c.universe._
        def foo: c.Expr[Int => Int => Int] =
          reify((x: Int) => (y: Int) => x + y)

        reify(foo.splice(1)(2))
      }
    }
  }

  // }}}

  implicit object LamPure_C extends LamPure[C] {
    import u._
    def lamS[a: TypeTag, b] = (fc: C[a] => C[b]) => new C[a => b] {
      def unC = vc => { // Expr[a => b]
        val name = "x_" + vc
        def body: Expr[b] = {
          fc(new C[a] {
            def unC = vc => C.mkExpr[a](q"${TermName(name)}")
          }) unC (vc + 1)
        }
        C.mkExpr[a => b](q"(${TermName(name)}: ${typeOf[a]}) => ${body}")
      }
    }
  }

  // ------------------------------------------------------------------------
  // Code combinators: generating code in an Applicative m
  // The code is pure but its generation may have an effect.

  // A Haskell value of the type
  // (Applicative m, SSym repr) => m (repr a)
  // represents a generator, in the generating applicative m,
  // of the expression in the object language of the type a

  def $$[repr[_]: SSym, m[_]: Applicative, a, b]: m[repr[a => b]] => m[repr[a]] => m[repr[b]] =
    mrab => mra => (mrab |@| mra) { implicitly[SSym[repr]].appS[a, b](_)(_) }

  def int[repr[_]: SSym, m[_]: Applicative]: Int => m[repr[Int]] = x => {
    implicitly[Applicative[m]].point { x |> implicitly[SSym[repr]].intS }
  }

  /*
infixl 2 $$
($$) :: (SSym repr, Applicative m) =>
        m (repr (a->b)) -> m (repr a) -> m (repr b)
f $$ x = appS <$> f <*> x

int :: (SSym repr, Applicative m) => Int -> m (repr Int)
int = pure . intS

infixl 7 *:
infixl 6 +:

(+:) :: (SSym repr, Applicative m) =>
        m (repr Int) -> m (repr Int) -> m (repr Int)
x +: y = pure addS $$ x $$ y

(*:) :: (SSym repr, Applicative m) =>
        m (repr Int) -> m (repr Int) -> m (repr Int)
x *: y = pure mulS $$ x $$ y

*/
}
