object sbtAvro {
  import sbt._
  import sbtavro.SbtAvro._

  private def deps(scaldingVersion: String) = Seq(
    "com.twitter" %% "scalding-core" % scaldingVersion
    , "com.twitter" %% "scalding-avro" % scaldingVersion
    )


  def settings(scaldingVersion: String) = avroSettings ++ Seq(
    Keys.libraryDependencies ++= deps(scaldingVersion)
    , Keys.version in avroConfig := "1.7.5" // remove this if you want cdh5 avro
                                            // to be pulled in
    , stringType in avroConfig := "String"
    )
}
