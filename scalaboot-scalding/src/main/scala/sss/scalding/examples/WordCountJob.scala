package sss.scalding.examples

import sss._
import scalding._

import com.twitter.scalding._
import TDsl._
import avro.PackedAvroSource

import WordCountJob._
import types.examples._

class WordCountJob(args: Args) extends HJob[TokenCount](args) {
  val output = TypedPipe.from(TextLine(inputPath)).
    flatMap { line: String => WordCountJob.tokenize(line) }.
    groupBy(identity).size.
    toTypedPipe.map {
      case (x, y) => new TokenCount(x, y)
    }

  output.write(PackedAvroSource[Output](outputPath))
}

class WordCountJobReader(args: Args) extends HJob[(String, Long)](args) {
  val output = PackedAvroSource[TokenCount](inputPath).
    map(x => (x.token, x.count))

  output.write(TypedTsv[Output](outputPath))
}

object WordCountJob {
  def tokenize(text: String): Seq[String] = text.split("""\s+""")
}
