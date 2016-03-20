object sbtScalariform {
  import sbt._
  import sbt.Keys._

  import com.typesafe.sbt.SbtScalariform._
  import scalariform.formatter.preferences._
  import scala.language.postfixOps

  private def configureScalariform(pref: IFormattingPreferences): IFormattingPreferences = {
    pref
      // Configure Scalariform according to the [Scala style
      // guide](https://github.com/daniel-trinh/scalariform#scala-style-guide)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  }


  /**
    * uncomment 'scalariformSettings' below with
    * 'defaultScalariformSettingsWithIt' to disable auto-format on compile. if
    * we're doing this then we should enforce that the hooks are run by some
    * other means (a possibility is via git pre-commit hooks)
    */
  lazy val settings: Seq[Setting[_]] = scalariformSettingsWithIt ++
    Seq(
      ScalariformKeys.preferences := configureScalariform(FormattingPreferences()),
      commands += Command.command("reformat-code-check-unchanged") { state =>
        if (("git diff --exit-code" !) != 0) {
          state.log.error("Run `sbt reformat-code` to fix Scala style formatting issues")
          sys.exit(1)
        }
        state
      }) ++
    addCommandAlias("reformat-code",
      ";compile:scalariformFormat;test:scalariformFormat;it:scalariformFormat") ++
    addCommandAlias("reformat-code-check", ";reformat-code;reformat-code-check-unchanged")
}
