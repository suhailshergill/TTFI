object sbtCompilerPlugins {
  import sbt._

  private object sbtKindProjector {
    lazy val settings = Seq(
      Keys.resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"
      , addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")
    )
  }

  lazy val settings = Seq(
    Keys.autoCompilerPlugins := true
    ) ++ sbtKindProjector.settings
}
