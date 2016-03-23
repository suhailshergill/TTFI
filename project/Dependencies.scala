import sbt._
import sbt.Keys._

object Dependencies {
  object Versions {
    lazy val scala211 = "2.11.7"
    lazy val cats = "0.4.1"
    lazy val scalatest = "2.2.6"
    lazy val scalacheck = "1.12.5"
    lazy val shapelessContrib = "0.4"
    lazy val spark = "1.6.1"
    lazy val predef = "master"

    implicit class Ops(s: String) {
      def asMM: String = s.split('.').toSeq.init.mkString(".")
    }
  }
  import Versions.Ops

  lazy val `org.scala-lang:scala-reflect` =
    "org.scala-lang" % "scala-reflect" % Versions.scala211
  lazy val `com.chuusai:shapeless` = "com.chuusai" %% "shapeless" % "2.2.0"
  lazy val `org.typelevel:shapeless-spire` =
    "org.typelevel" %% "shapeless-spire" % Versions.shapelessContrib
  lazy val `org.typelevel:cats` = "org.typelevel" %% "cats" % "0.4.1"
  lazy val `org.specs2:eff-cats` = "org.specs2" %% "eff-cats" % "1.1"
  lazy val `com.github.mpilquist:simulacrum` =
    "com.github.mpilquist" %% "simulacrum" % "0.7.0"
  lazy val `org.typelevel:discipline` = "org.typelevel" %% "discipline" % "0.4"


  lazy val `org.spire-math:spire` = "org.spire-math" %% "spire" % "0.8.2"
  lazy val `org.apache.spark:spark-core` =
    "org.apache.spark" %% "spark-core" % Versions.spark excludeSparkDependencies()
  lazy val `org.apache.spark:spark-streaming` =
    "org.apache.spark" %% "spark-streaming" % Versions.spark excludeSparkDependencies()
  lazy val `org.apache.spark:spark-mllib` =
    "org.apache.spark" %% "spark-mllib" % Versions.spark excludeSparkDependencies()
  lazy val `org.apache.spark:spark-sql` =
    "org.apache.spark" %% "spark-sql" % Versions.spark excludeSparkDependencies()

  implicit class RichModuleId(m: ModuleID) {

    def excludeSparkDependencies(): ModuleID =
      m.
        exclude("commons-beanutils", "commons-beanutils-core").
        exclude("commons-collections", "commons-collections").
        exclude("commons-logging", "commons-logging").
        exclude("org.slf4j", "slf4j-log4j12").
        exclude("org.hamcrest", "hamcrest-core").
        exclude("junit", "junit").
        exclude("org.jboss.netty", "netty").
        exclude("com.esotericsoftware.minlog", "minlog")
  }

  lazy val `org.specs2:specs2-core` = "org.specs2" %% "specs2-core" % "3.7.2" % "it,test"
  lazy val `org.scalacheck:scalacheck` = "org.scalacheck" %% "scalacheck" % Versions.scalacheck % "test,it"
  lazy val `com.github.alexarchambault:scalacheck-shapeless` =
    "com.github.alexarchambault" %%
      ("scalacheck-shapeless_" + Versions.scalacheck.asMM) %
      "0.3.1"

  /*
   * Dependency groups.
   */

  lazy val specs2: Def.Setting[_] = libraryDependencies ++= Seq(
    `org.specs2:specs2-core`
    , `org.scalacheck:scalacheck`
    , `com.github.alexarchambault:scalacheck-shapeless`
  )

  lazy val base: Def.Setting[_] = libraryDependencies ++= Seq(
    `org.scala-lang:scala-reflect`
    , `com.chuusai:shapeless`
    , `org.typelevel:shapeless-spire`
    , `org.typelevel:cats`
    // , `org.specs2:eff-cats`
    , `com.github.mpilquist:simulacrum`
    , `org.typelevel:discipline`
  )

  lazy val numbers: Def.Setting[_] = libraryDependencies ++= Seq(
    `org.spire-math:spire`
    , `org.apache.spark:spark-core`
    , `org.apache.spark:spark-streaming`
    , `org.apache.spark:spark-mllib`
    , `org.apache.spark:spark-sql`
  )

  lazy val `sss.predef` = RootProject(uri("git://github.com/suhailshergill/predef.git#%s".format(Versions.predef)))

}
