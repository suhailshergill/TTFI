package finals

object PushNeg {
  // make the context on which the operation depends explicit
  sealed trait Ctx
  case object Pos extends Ctx
  case object Neg extends Ctx

  // [[http://stackoverflow.com/a/8736360][typed lambdas]]
  // this trait allows us to provide nicer type signatures. without this,
  // instead of Ctx_=>[repr]#τ we'd be using something like the following
  // ({ type λ[T] = Ctx => repr[T] })#λ
  //
  // alternatively, use <https://github.com/non/kind-projector> with which this
  // type would be described as 'λ[A => (Ctx => repr[A])]'. see
  // <https://github.com/non/kind-projector#gotchas> for why the anonymous
  // syntax can't be used.
  trait Ctx_=>[repr[_]] {
    type τ[T] = Ctx => repr[T]
  }

  // PushNeg.apply === pushNeg (in Initial version). pass in the 'no-op'/
  // base context
  def apply[repr[_]](e: Ctx => repr[Integer]): repr[Integer] = e(Pos)

  import ExpSym._
  // what we'd like is something like the following:
  // implicit object ExpSym_Ctx[repr](implicit s1: ExpSym[repr]) extends ExpSym[Ctx_=>[repr]#τ]
  //
  // due to limitation that scala objects need to have a concrete type, this
  // needs to be an 'implicit class' or an 'implicit def'
  implicit def ExpSym_Ctx[repr[_]](implicit e: ExpSym[repr]) = new ExpSym[Ctx_=>[repr]#τ] {
    def lit = (x: Integer) => (ctx: Ctx) => ctx match {
      case Pos => e.lit(x)
      case Neg => e.neg(e.lit(x))
    }
    def neg = x => (ctx: Ctx) => ctx match {
      case Pos => x(Neg)
      case Neg => x(Pos)
    }
    def add = x => y => (ctx: Ctx) => e.add(x(ctx))(y(ctx))
  }

  import MulSym._
  implicit def MulSym_Ctx[repr[_]](implicit m: MulSym[repr]) = new MulSym[Ctx_=>[repr]#τ] {
    def mul = x => y => (ctx: Ctx) => ctx match {
      case Pos => m.mul(x(Pos))(y(Pos))
      case Neg => m.mul(x(Pos))(y(Neg))
    }
  }
}
