package sss.finals

import org.specs2._
class IntegersSpecs extends Specification {
  def is = s2"""
  ExpSym  $es
  MulSym  $ms
"""
  import Exp._
  import ExpSym._

  def es = {

    // TODO: obviate need to pass type explicitly
    // in haskell, the type of 'eval' tells the compiler which type dictionary
    // to look at. it can do that because overlapping instances aren't allowed
    // by default. in scala, however, you can have multiple overlapping
    // implicits. the dispatch mechanism then is based on
    // <http://stackoverflow.com/questions/5598085/where-does-scala-look-for-implicits>
    // what we need is a way to thread a type to underlying argument
    def tf1[repr[_]](implicit e: ExpSym[repr]): repr[Integer] = {
      import e._
      add(lit(8))(neg(add(lit(1))(lit(2))))
    }

    def tf2[repr[_]](implicit e: ExpSym[repr]): repr[Integer] = {
      import e._
      neg(tf1(e))
    }

    eval(tf1[Eval]) ==== 5 &&
      view(tf1[Debug]) ==== "(8 + (-(1 + 2)))"
  }

  def ms = {

    import MulSym._

    def tfm1[repr[_]](implicit e: ExpSym[repr], m: MulSym[repr]) = {
      import e._; import m._
      add(lit(8))(neg(mul(lit(1))(lit(2))))
    }

    eval(tfm1[Eval]) ==== 6 &&
      view(tfm1[Debug]) ==== "(8 + (-(1 * 2)))"
  }

  // def ms2 = {
  //   classOf[Long] ==== Numeric.NumSym.result1.getClass &&
  //     0L ==== Numeric.NumSym.result1
  // }
}
