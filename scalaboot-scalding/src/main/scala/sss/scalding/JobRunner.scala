package sss.scalding

import org.apache.hadoop
import com.twitter.scalding.Tool

/**
 * Entrypoint for Hadoop to kick off the job.
 * example use:
 * hadoop jar scalaboot-scalding-<version>.jar <job-class> --hdfs --input <inputfile> --output <outputfile>
 *
 * Borrowed from com.twitter.scalding.Tool
 */
object JobRunner {
  def main(args: Array[String]) {
    System.out.println("Starting JobRunner ...")
    hadoop.util.ToolRunner.run(new hadoop.conf.Configuration, new Tool, args);
  }
}
