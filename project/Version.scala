sealed trait Version

object Version {
  import sbt._
  import sbt.Keys._

  import scala.language.postfixOps

  case object SNAPSHOT extends Version
  case object RELEASE extends Version

  def set(versionNumber: String, versionType: Version): Setting[_] =
    version in ThisBuild := (versionType match {
      case RELEASE =>  versionNumber
      case SNAPSHOT => {
        s"${versionNumber}-" + ("git rev-parse HEAD" !!).trim + "-SNAPSHOT"
      }
    })
}
