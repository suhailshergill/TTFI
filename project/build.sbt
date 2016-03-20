lazy val build = project.in(file(".")).
  settings(scalacOptions ++= Seq("-feature", "-Xfatal-warnings"))
