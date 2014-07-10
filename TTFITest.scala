import org.scalatest._

class foo extends WordSpec {
  "TTFI" should {
    import TTFI._

    "ExpSym" in {
      println(Final.ExpSym.Use.result)
      println(Final.ExpSym.Use.result2)
    }

    "MulSym" in {
      println(Final.MulSym.Use.result)
      println(Final.MulSym.Use.result2)
    }

    "TreeSem" in {
      assert(Final.TreeSem.Use.result.isRight)
      assert(Final.TreeSem.Use.result2.isRight)
      assert(Final.TreeSem.Use.result3.isLeft)
      assert(Final.TreeSem.Use.result4.isRight)

      assertResult(Final.TreeSem.Use.result2) {
        Final.TreeSem.Use.result2b
      }
      assertResult(Final.TreeSem.Use.result4) {
        Final.TreeSem.Use.result4b
      }
    }

    "PushNeg" in {
      println(Final.PushNeg.Use.result)
      println(Final.PushNeg.Use.result2)
      assert(Initial.FP.result == Final.PushNeg.Use.result)
    }

  }
}