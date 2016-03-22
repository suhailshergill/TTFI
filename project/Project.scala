import sbt._
import Keys._
import Dependencies._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging

object TTFIBuild extends Build {
  val ORG_NAME = "Suhail Shergill"
  val ORG = "sss"
  val PROJECT_NAME = "ttfi"


  lazy val defaultSettings = Defaults.itSettings ++
    sbtCompilerPlugins.settings ++
    sbtScalariform.settings ++
    sbtScoverage.settings ++
    sbtWartremover.settings ++
    scalacOptions.settings ++
    Seq(
      CommonResolvers.settings,
      base, specs2,
      retrieveManaged := true,
      publishMavenStyle := true,
      organization in ThisBuild := ORG,
      organizationName in ThisBuild := ORG_NAME,
      Version.set("0.1", Version.SNAPSHOT),
      scalaVersion in ThisBuild := Versions.scala211,
      fork in Test := true,
      // Show current project name in the SBT prompt, e.g. `pythia>`
      shellPrompt in ThisBuild := { state =>
        Project.extract(state).currentRef.project + "> " }
    )

  def defaultProject: Project => Project = _.
    configs(IntegrationTest).
    settings(defaultSettings: _*).
    enablePlugins(JavaAppPackaging).
    settings(testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Specs")))).
    settings(testOptions in IntegrationTest := Seq(Tests.Filter(s => s.endsWith("Specs")))).
    settings(parallelExecution in IntegrationTest := false)


  lazy val root = defaultProject(Project(PROJECT_NAME, file(".")))
    .aggregate(core)

  lazy val core = defaultProject(Project(PROJECT_NAME+"-core", file(PROJECT_NAME+"-core")))
}
