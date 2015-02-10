object sbtScalariform {
  import sbt._

  import com.typesafe.sbt.SbtScalariform._
  import scalariform.formatter.preferences._

  private def configureScalariform(pref: IFormattingPreferences): IFormattingPreferences = {
    pref
      .setPreference(AlignParameters, true)
  }


  lazy val settings = scalariformSettings ++
    Seq(
      ScalariformKeys.preferences := configureScalariform(FormattingPreferences())
    )
}
