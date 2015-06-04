package finals.staged

/**
 * scala port of <http://okmij.org/ftp/tagless-final/TaglessStaged/TSCore.hs>
 */
object TSCore {

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
  def runCS[a]: C[a] => String = cs => cs.unC(0).tree.toString

  // Adding `purely generated' lambda-expressions to our EDSL
  // Any effect of generating their body can't propagate past
  // the binder.
  // See below for more general lambda-expressions
  trait LamPure[repr[_]] {
    def lamS[a, b]: (repr[a] => repr[b]) => repr[a => b]
  }

  implicit object Lampure_R extends LamPure[R] {
    def lamS[a, b] = rf => R(x => {
      (rf apply R(x)).unR
    })
  }

  // {{{ scratch

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context
  def hmm_impl(c: Context): c.Expr[Int] = {
    import c.universe._
    def foo: c.Expr[Int => Int => Int] =
      reify((x: Int) => (y: Int) => x + y)

    reify(foo.splice(1)(2))
  }

  def add = ((x: Int) => (y: Int) => x + y)
  def addR: u.Expr[Int => Int => Int] = u reify add
  def showExpr[a]: u.Expr[a] => String = e => u showRaw e.tree
  def hmm = u.showRaw(u.reify((x: Int) => (y: Int) => x + y))
  def qu: u.Expr[Int] = u reify { add(1)(2) }
  def qux: u.Tree = qu.tree // 5
  def qux2: String = u showRaw qux // Literal(Constant(5))

  // }}}

  /*
  The connection to scala macros

  Unlike scala macros, based on reification and splicing for building
  code values, we use combinators for code generation (cf. Xi and Thiemann).
  Here is how our combinators could be defined in terms of
  reification and splicing:

  intS x  =  reify(x)
  addS    =  reify((x: Int) => (y: Int) => x + y)
  appS x y = reify(x.splice apply y.splice)

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
      def unC = const(
        u reify ((x: Int) => (y: Int) => x + y))
    }
    def mulS = new C[Int => Int => Int] {
      def unC = const(
        u reify ((x: Int) => (y: Int) => x * y))
    }
    def appS[a, b] = cf => cx => new C[b] {
      def unC = vc => u reify (
        cf.unC(vc).splice apply cx.unC(vc).splice)
    }
  }

  object Scratch {
    import u._
    type Code[T] = Expr[T]

    // def eta[a, b]: (Code[a] => Code[b]) => Code[a => b] = f => {
    //   reify((x: a) => f(reify(x)).splice)
    // }
  }

  implicit object LamPure_C extends LamPure[C] {
    def lamS[a, b] = fc => new C[a => b] {
      def unC = vc => { // Expr[a => b]
        import u._
        val name = "x_" + vc
        def lm: Function = q"(${name}: a) => ${name}"
        def baz: Typed = q"(x: Int)"
        def lm2: Expr[Function] = reify(q"(x: Intsdfldfkj) => x")
        def lm3: Expr[Int => Int] = reify((x: Int) => x)
        def hmm: Expr[Function] = reify(lm)
        /*
instance LamPure C where
    lamS f = C $ \vc ->
         let name = mkName $ "x_" ++ show vc
             body = unC (f (C . const $ VarE name)) (succ vc)
             in LamE [VarP name] body
*/

        // Function needs the following args:
        // (vparams: List[reflect.runtime.universe.ValDef], body: reflect.runtime.universe.Tree)
        def vparams: List[u.ValDef] = ???
        def body: u.Tree = ???
        def func: u.Function = u.Function(vparams, body)
        //import c.universe._
        def foo = u.Literal(u.Constant("x"))
        def bar = {
          import u._
          val five: Expr[Int] = reify(5)
          val fiveTree: Tree = five.tree
          //val hmm = fiveTree.eval
          ???
        }

        // so how do we create ValDef ("VarP")
        // - ValDef requires u.Symbol
        //val valdef = u.ValDef(u.Symbol("x_" + vc))

        // how do we use ValDef in body? ("VarE")

        ???
      }
    }
  }

}
