package finals

import initial.FP.{ Exp => Initial, Lit, Neg, Add }

object Isomorphic {
  import ExpSym.ExpSym

  /**
   * map final forms to initial representations. final forms are flexible and
   * are generic in representation domains. the initial representation is merely
   * another representation domain being thrown into the mix. the signature of
   * 'initialize' below ensures that the result of calling initialize is in the
   * initial representation; the flexibility of final representation ensures
   * that we can call initialize on final forms provided we have the implicit
   * for 'ExpSym[Initial]' (i.e., 'ExpSym_Initial') in scope.
   */
  def initialize: Initial[Integer] => Initial[Integer] = identity

  /**
   * map initial forms into final forms. this simply invokes 'Exp_Final' def
   */
  def finalize[repr[_]: ExpSym](x: Initial[Integer]): repr[Integer] = Exp_Final[repr](x)

  implicit object ExpSym_Initial extends ExpSym[Initial] {
    def lit = (x: Integer) => Lit(x)
    def neg = x => Neg(x)
    def add = x => y => Add(x, y)
  }

  implicit def Exp_Final[repr[_]](x: Initial[Integer])(implicit s: ExpSym[repr]): repr[Integer] =
    x match {
      case Lit(e) => s.lit(e)
      case Neg(e) => s.neg(Exp_Final(e)(s))
      case Add(e1, e2) => s.add(Exp_Final(e1)(s))(Exp_Final(e2)(s))
    }
}
