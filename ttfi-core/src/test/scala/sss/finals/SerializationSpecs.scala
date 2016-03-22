package sss.finals

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
    val result: Either[ErrMsg, String] = ClosedRecursion.fromTree[Debug](tf1_tree).right.map(view)
    val result2: Either[ErrMsg, String] = OpenRecursion.fromTree[Debug](tf1_tree).right.map(view)
    def result2a[repr[_]](implicit s1: ExpSym[repr]): Either[ErrMsg, repr[Integer]] = {
      OpenRecursion.fromTree[repr](tf1_tree)
    }
    val result2b: Either[ErrMsg, String] = result2a[Debug].right.map(view)

    val result3: Either[ErrMsg, String] = ClosedRecursion.fromTreeExt[Debug](tfm1_tree).right.map(view)
    val result4: Either[ErrMsg, String] = OpenRecursion.fromTreeExt[Debug](tfm1_tree).right.map(view)
    def result4a[repr[_]](implicit s1: ExpSym[repr], s2: MulSym[repr]) = {
      OpenRecursion.fromTreeExt[repr](tfm1_tree)
    }
    val result4b: Either[ErrMsg, String] = result4a[Debug].right.map(view)

    val tf1_int3 = {
      import OpenRecursion._
      // FIXME: make it easier to construct these implicits
      implicit def ExpSym_Debug_Tree: ExpSym[(Debug :> Tree)#τ] = ExpSym_Dup[Debug, Tree]
      implicit def ExpSym_Eval_Debug_Tree: ExpSym[(Eval :> (Debug :> Tree)#τ)#τ] = ExpSym_Dup[Eval, (Debug :> Tree)#τ]

      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
        check_consume(thrice) _ apply fromTree[(Eval :> (Debug :> Tree)#τ)#τ](tf1_tree)
      }
      stream.toString
    }

    result.isRight &&
      result2.isRight &&
      result3.isLeft &&
      result4.isRight &&
      result2 ==== result2b &&
      result4 ==== result4b &&
      tf1_int3 ==== """5
(8 + (-(1 + 2)))
Node(Add,List(Node(Lit,List(Leaf(8))), Node(Neg,List(Node(Add,List(Node(Lit,List(Leaf(1))), Node(Lit,List(Leaf(2)))))))))
"""
  }
}
