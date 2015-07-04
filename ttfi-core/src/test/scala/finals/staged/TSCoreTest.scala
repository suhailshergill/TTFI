package finals.staged

import org.specs2.mutable.Specification

class TSCoreTest extends Specification {
  import TSCore._
  import scalaz._
  import Scalaz._
  "SSym should allow you to" >> {
    "define terms and" >> {
      def exS1[repr[_]: SSym]: repr[Int] = {
        val ev = implicitly[SSym[repr]]
        import ev._
        addS |> $(intS(1)) |> $(intS(2))
      }
      "evaluate them in C[_] repr" in {
        runCS(exS1[C]) ===
          "((x: Int) => ((y: Int) => x.$plus(y))).apply(x).apply(x)"
      }
    }
    "define LamPure in C and" >> {
      def exS2[repr[_]](implicit ev1: SSym[repr], ev2: LamPure[repr]): repr[Int => Int => Int] = {
        import scala.reflect.runtime.{ universe => u }
        import ev1._; import ev2._

        type T = repr[Int]
        implicit val ev3 = implicitly[u.TypeTag[Int]]
        lamS apply { (x: T) =>
          lamS apply { (y: T) =>
            addS |> $(x) |> $(y)
          }
        }
      }
      "evaluate them in C[_] repr" in {
        runCS(exS2[C]) ==
          "((x_0: Int) => ((x_1: Int) => ((x: Int) => ((y: Int) => x.$plus(y))).apply(x_0).apply(x_1)))"
      }
    }
  }

  "graph library should" >> {
    import scalax.collection.Graph // or scalax.collection.mutable.Graph
    import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

    import scalax.collection.Graph, scalax.collection.GraphEdge._
    import scalax.collection.io.dot._
    import implicits._
    import scalax.collection.edge.LDiEdge, scalax.collection.edge.Implicits._

    val graph = Graph(3 ~ 1, 5)

    import Indent._
    import scala.language.existentials

    implicit def toLDiEdge[N](diEdge: DiEdge[N]) = LDiEdge(diEdge._1, diEdge._2)("")
    val g = Graph[String, LDiEdge](
      ("A1" ~+> "A2")("f"), ("A2" ~+> "A3")("g"), "A1" ~> "B1", "A1" ~> "B1", ("A2" ~+> "B2")("(g o f)'"), "A3" ~> "B3", "B1" ~> "B3", ("B2" ~+> "B3")("g'"))
    val root = DotRootGraph(directed = true,
      id = Some(Id("Wikipedia_Example")))
    val subA = DotSubGraph(ancestor = root,
      subgraphId = Id("A"),
      attrList = List(DotAttr(Id("rank"), Id("same"))))
    val subB = DotSubGraph(ancestor = root,
      subgraphId = Id("B"),
      attrList = List(DotAttr(Id("rank"), Id("same"))))
    def edgeTransformer(innerEdge: Graph[String, LDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
      val edge = innerEdge.edge
      val label = edge.label.asInstanceOf[String]
      Some((root,
        DotEdgeStmt(NodeId(edge.from.toString),
          NodeId(edge.to.toString),
          if (label.nonEmpty) List(DotAttr(Id("label"), Id(label)))
          else Nil)))
    }
    def nodeTransformer(innerNode: Graph[String, LDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] =
      Some((if (innerNode.value.head == 'A') subA else subB,
        DotNodeStmt(NodeId(innerNode.toString), Seq.empty[DotAttr])))
    val dot = g.toDot(dotRoot = root,
      edgeTransformer = edgeTransformer,
      cNodeTransformer = Some(nodeTransformer),
      spacing = Spacing(TwoSpaces))

    "print dot repr" in {
      println(dot)
      // write to file
      import java.nio.file.{ Paths, Files }
      import java.nio.charset.StandardCharsets
      Files.write(Paths.get("foo.dot"), dot.getBytes(StandardCharsets.UTF_8))
      //
      pending
    }
  }
}
