package finals

import org.specs2._
class SerializationSpecs extends Specification {
  def is = s2"""
  TreeSem $ts
"""

  def ts = {
    import Exp._
    import ExpSym._
    import MulSym._
    import TreeSem._

    def tf1[repr[_]](implicit e: ExpSym[repr]): repr[Integer] = {
      import e._
      add(lit(8))(neg(add(lit(1))(lit(2))))
    }

    def tfm1[repr[_]](implicit e: ExpSym[repr], m: MulSym[repr]) = {
      import e._; import m._
      add(lit(8))(neg(mul(lit(1))(lit(2))))
    }

    val tf1_tree = toTree(tf1[Tree])
    val tfm1_tree = toTree(tfm1[Tree])

    // deserialization problem is the problem that we're unable to have a truly
    // generic representation (while allowing for language extensibility). the
    // symptom is having to invoke 'fromTree' twice for two uses. if the
    // language were fixed, what we could do is to create a wrapper
    // representation incorporating all the relevant typeclasses (language
    // constructs). what the duplicating interpretor does is to have these
    // trampoline-like constructs which lazily (per use) invoke, in essence, the
    // fromTree translation (via the ExpSym instance for the 'repr' pair)
    val result = ClosedRecursion.fromTree[Debug](tf1_tree).right.map(view)
    val result2 = OpenRecursion.fromTree[Debug](tf1_tree).right.map(view)
    // def tf1_tree_parse[repr[_]] = OpenRecursion.Poly2.fromTree[repr](tf1_tree)
    def result2a[repr[_]](implicit s1: ExpSym[repr]): Either[ErrMsg, repr[Integer]] = {
      OpenRecursion.Poly.fromTree[repr](tf1_tree)
      // tf1_tree_parse[repr].right.map(_(s1))
    }
    val result2b = result2a[Debug].right.map(view)

    val result3 = ClosedRecursion.fromTreeExt[Debug](tfm1_tree).right.map(view)
    val result4 = OpenRecursion.fromTreeExt[Debug](tfm1_tree).right.map(view)
    def result4a[repr[_]](implicit s1: ExpSym[repr], s2: MulSym[repr]) = {
      OpenRecursion.Poly.fromTreeExt[repr](tfm1_tree)
    }
    val result4b = result4a[Debug].right.map(view)

    result.isRight &&
      result2.isRight &&
      result3.isLeft &&
      result4.isRight &&
      result2 ==== result2b &&
      result4 ==== result4b
  }
}
