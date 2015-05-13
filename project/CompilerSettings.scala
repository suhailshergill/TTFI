import sbt._
import Keys._

object CompilerSettings {
	lazy val settings: Seq[Def.Setting[_]] = Seq(
			scalacOptions ++= Seq(
			"-deprecation",
			"-explaintypes",
		  "-encoding", "UTF-8",
		  "-feature",
		  "-language:higherKinds",
		  "-language:implicitConversions",
		  "-unchecked",
		  "-Xfatal-warnings",
		  "-Xlint",
		  "-Yno-adapted-args",
		  "-Ywarn-dead-code",
		  "-Ywarn-numeric-widen",
		  "-Ywarn-value-discard",
		  "-Xfuture"
		)
	)
}