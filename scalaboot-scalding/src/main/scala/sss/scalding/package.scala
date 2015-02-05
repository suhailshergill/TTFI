package sss

import com.twitter.scalding._

package object scalding {
  type FlowDef = cascading.flow.FlowDef
  type Pipe = cascading.pipe.Pipe

  type TP[A] = TypedPipe[A]

  // base job
  trait HJobT[T] {
    type Output = T
    def inputPath: String
    def outputPath: String
  }

  class HJob[T](args: Args) extends Job(args) with HJobT[T] {
    def inputPath = args("input")
    def outputPath = args("output")

    override def ioSerializations = {
      super.ioSerializations :+
        classOf[cascading.avro.serialization.AvroSpecificRecordSerialization[_]]
    }
  }

  trait HJobTest[T] extends HJobT[T] {
    def inputPath = "inputDS"
    def outputPath = "outputDS"

    def testJob(jobName: String): JobTest = {
      JobTest(jobName)
        .arg("input", inputPath)
        .arg("output", outputPath)
    }
  }

}
