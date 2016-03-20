object sbtScoverage {
  import sbt._
  import sbt.Keys._

  import scoverage.ScoverageKeys._

  def settings: Seq[Setting[_]] = Seq(
    coverageEnabled in ThisBuild := true
    , coverageMinimum in ThisBuild := 70
    , coverageFailOnMinimum in ThisBuild := true
    , coverageHighlighting in ThisBuild := true
    , coverageOutputHTML in ThisBuild := true
  )

}
