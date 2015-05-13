package finals

object Flattening {
  // make the context on which the operation depends explicit
  sealed trait LCACtx[+repr[_]]
  case object NonLCA extends LCACtx[Nothing]
  case class LCA[+repr[_]](e: repr[Integer]) extends LCACtx[repr]

  // instance ExpSYM repr ⇒ ExpSYM (LCACtx → repr) where
  // instance ExpSYM repr ⇒ ExpSYM (LCACtx repr → repr) where

  // [[http://stackoverflow.com/a/8736360][typed lambdas]]
  // this trait allows us to provide nicer type signatures. without this,
  // instead of LCACtx_=>[repr]#τ we'd be using something like the following
  // ({ type λ[T] = LCACtx => repr[T] })#λ
  // alternatively, use <https://github.com/non/kind-projector>
  trait LCACtx_=>[repr[_]] {
    type τ[T] = LCACtx[repr] => repr[T]
  }

  def apply[repr[_]](e: LCACtx[repr] => repr[Integer]): repr[Integer] = e(NonLCA: LCACtx[repr])

  import ExpSym._
  // what we'd like is something like the following:
  // implicit object ExpSym_LCACtx[repr](implicit s1: ExpSym[repr]) extends ExpSym[LCACtx_=>[repr]#τ]
  //
  // due to limitation that scala objects need to have a concrete type, this
  // needs to be an 'implicit class' or an 'implicit def'. directly declaring an
  // 'implicit def' runs afoul of scala's typechecker so we separate the
  // implicit activation (i.e., the implicit def) from the actual definition.
  implicit def ExpSym_Flat_LCACtx[repr[_]](implicit e: ExpSym[repr]) = new ExpSym_Flat_LCACtx[repr]
  class ExpSym_Flat_LCACtx[repr[_]](implicit e: ExpSym[repr]) extends ExpSym[LCACtx_=>[repr]#τ] {
    import e._
    def lit = (x: Integer) => (ctx: LCACtx[repr]) => ctx match {
      case NonLCA => e.lit(x)
      case LCA(v) => e.add(e.lit(x))(v)
    }
    def neg = x => (ctx: LCACtx[repr]) => ctx match {
      case NonLCA => e.neg(x(NonLCA))
      case LCA(v) => e.add(e.neg(x(NonLCA)))(v)
    }
    def add = x => y => (ctx: LCACtx[repr]) => x(LCA(y(ctx)))
  }
}
