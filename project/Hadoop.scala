object Hadoop {
  import sbt._

  val scaldingVersion = "0.13.1"
  val JOBRUNNER = "sss.scalding.JobRunner"

  private lazy val resolvers = Seq( // scalding, cascading etc
    "Concurrent Maven Repo" at "http://conjars.org/repo",
    "clojars.org" at "http://clojars.org/repo")

  private lazy val deps = Seq(
    "com.twitter" %% "scalding-commons" % scaldingVersion
    // "org.apache.hadoop" % "hadoop-core" % "2.3.0-mr1-cdh5.0.1" % "provided",
  )

  def settings(defaultSettings: Seq[Def.Setting[_]]) = defaultSettings ++
    sbtAssembly.settings(Hadoop.JOBRUNNER) ++
    sbtAvro.settings(Hadoop.scaldingVersion) ++
    Seq(
      Keys.resolvers ++= resolvers
      , Keys.libraryDependencies ++= deps
    )
}
