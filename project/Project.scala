import sbt._
import Keys._
import LogSettings._

//TODO change the name of the object to reflect your project name.
object TTFIBuild extends Build {
  val ORG_NAME = "sss" // TODO change this!
  val PROJECT_NAME = "ttfi" //TODO change this!

  lazy val commonResolvers = Seq(
    "Maven.org" at "http://repo1.maven.org/maven2"
    , "Sun Maven2 Repo" at "http://download.java.net/maven/2"
    , "Scala-Tools" at "http://scala-tools.org/repo-releases/"
    , "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish"
    , "Oracle Maven2 Repo" at "http://download.oracle.com/maven"
    , "spy" at "http://files.couchbase.com/maven2/"
    , "Twitter" at "http://maven.twttr.com/"
    , Resolver.sonatypeRepo("releases")
    , Resolver.typesafeRepo("releases")
  )

  lazy val commonDeps = Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "com.chuusai" %% "shapeless" % "2.0.0",
    "org.specs2" %% "specs2-core" % "2.4.15" % "test, it"
  )

  lazy val defaultSettings = 
    Defaults.itSettings ++
    CompilerSettings.settings ++
    logSettings ++
    sbtCompilerPlugins.settings ++
    sbtStartScript.settings ++
    sbtScalariform.settings ++
    Seq(
      libraryDependencies ++= commonDeps,
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      resolvers ++= commonResolvers,
      retrieveManaged := true,
      publishMavenStyle := true,
      organization := ORG_NAME,
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.5"
    )

  def defaultProject: Project => Project = _.
    configs(IntegrationTest).
    settings(defaultSettings: _*).
    settings(testOptions in IntegrationTest := Seq(Tests.Filter(s => s.contains("Test")))).
    settings(parallelExecution in IntegrationTest := false)


  lazy val root = defaultProject(Project(PROJECT_NAME, file(".")))
    .aggregate(core, scalding)

  lazy val core = defaultProject(Project(PROJECT_NAME+"-core", file(PROJECT_NAME+"-core")))

  lazy val scalding = defaultProject(Project(s"${PROJECT_NAME}-scalding", file(s"${PROJECT_NAME}-scalding")))
    .settings(Hadoop.settings(defaultSettings): _*)
    .dependsOn(core)
}
