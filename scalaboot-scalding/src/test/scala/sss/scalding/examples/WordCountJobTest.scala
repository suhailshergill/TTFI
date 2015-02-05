package sss.scalding.examples

import sss._
import sss.scalding._
import types.examples._

import org.scalatest._

import com.twitter.scalding._
import avro.PackedAvroSource
import TDsl._

import WordCountJob._

class WordCountJobTest extends WordSpec with HJobTest[TokenCount] {
  val inputData = scala.io.Source.fromURL(getClass.getResource("/hack.txt")).
    getLines.zipWithIndex.map(_.swap).toIterable

  "WordCountJob" should {
    testJob("sss.scalding.examples.WordCountJob").
      source(TextLine(inputPath), inputData).
      sink[Output](PackedAvroSource[Output](outputPath))(buf => {
        val outMap = buf.map(x => (x.token, x.count)).toMap
        "count words correctly" in {
          assertResult(4) {
            outMap("hack")
          }
          assertResult(1) {
            outMap("and")
          }
        }
      }).
      run.
      finish
  }
}
