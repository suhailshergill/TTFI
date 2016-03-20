object scalacOptions {
  import sbt._
  lazy val settings: Seq[Setting[Task[Seq[String]]]] = Seq(
    Keys.scalacOptions in ThisBuild ++= Seq("-deprecation"
      , "-encoding", "UTF-8"
      , "-explaintypes"
      , "-feature"
      , "-language:existentials"
      , "-language:higherKinds"
      , "-language:implicitConversions"
      , "-language:experimental.macros"
      , "-target:jvm-1.7"
      , "-unchecked"
      , "-Xexperimental"
      , "-Xfatal-warnings"
      , "-Xfuture"
      , "-Xlint"
      , "-Yno-adapted-args"
      , "-Ywarn-dead-code"
      , "-Ywarn-numeric-widen"
      , "-Ywarn-value-discard"
    )
    // http://spark.apache.org/docs/latest/building-spark.html#building-with-buildmvn
    , Keys.scalacOptions in Compile ++= Seq("-Xmax-classfile-name", "128")
    , Keys.scalacOptions in Test ++= Seq("-Yrangepos") // recommended in specs2 doc
    // , Keys.scalacOptions in Compile ++= Seq("-Xprint-types", "-Xprint:typer")
    )
}
