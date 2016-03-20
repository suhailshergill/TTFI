object sbtCompilerPlugins {
  import sbt._

  private object sbtKindProjector {
    lazy val settings = Seq(
      Keys.resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"
      , addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
    )
  }

  private object sbtParadise {
    lazy val settings = Seq(
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross
        CrossVersion.full)
    )
  }

  lazy val settings = Seq(
    Keys.autoCompilerPlugins := true
    ) ++
    sbtKindProjector.settings ++
    sbtParadise.settings
}
