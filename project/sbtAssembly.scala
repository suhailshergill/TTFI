object sbtAssembly {
  import sbt._
  import Keys._

  import sbtassembly.Plugin._
  import AssemblyKeys._

  def settings(mainClassName: String) =  assemblySettings ++ Seq(
    // Slightly cleaner jar name
    jarName in assembly := { name.value + "-" + version.value + ".jar" },
    test in assembly := {}, // ignore tests
    mainClass in assembly := Some(mainClassName),
    // NOTE: specifying the main class for Compile,run is currently somewhat
    // buggy because of this
    // [[https://github.com/sbt/sbt/issues/850][bug]]. it's also not really
    // needed but adding here for completeness
    mainClass in (Compile, run) := Some(mainClassName),
    // Drop these jars
    excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
      val excludes: Set[String] = Set(
        "minlog-1.2.jar" // Otherwise causes conflicts with Kryo (which bundles it)
        , "commons-beanutils-core-1.8.0.jar" // Clash with each other and with commons-collections
        , "commons-beanutils-1.7.0.jar" // "
        , "asm-3.1.jar" // there's already asm-4.0
        , "jsp-2.1-6.1.14.jar"
      )
      cp filter { jar => excludes(jar.data.getName) }
    },

    mergeStrategy in assembly <<= (mergeStrategy in assembly) {
      (old) =>
        {
          case PathList("javax", "servlet", xs @ _*) => MergeStrategy.first
          case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
          case "project.clj" => MergeStrategy.discard // Leiningen build files
          case x => old(x)
        }
    })
}
