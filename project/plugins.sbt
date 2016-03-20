// the below doesn't seem to be needed, but adding here for completeness if
// nothing else
resolvers += Resolver.sbtPluginRepo("releases")

// scalariform
resolvers += Resolver.typesafeRepo("releases")
addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

// supersafe
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0-RC6")

// native-packager
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.1.0-RC1")


// TODO: use sbt-mediative - will remove scalacOptions and scalariform settings
// and will add a few other conveniences. currently sbt-mediative is for 2.10 so
// that's why we're not using it. additionally, with all the options inlined as
// they are right now, it makes it easier to configure/test etc with settings
// (greater likelihood of that happening in the nascency of the project).

// resolvers += Resolver.url("YPG-Data SBT Plugins",
//   url("https://dl.bintray.com/ypg-data/sbt-plugins"))(Resolver.ivyStylePatterns)
// addSbtPlugin("com.mediative.sbt" % "sbt-mediative-core" % "0.1.1")


// wartremover
addSbtPlugin("org.brianmckenna" % "sbt-wartremover" % "0.14")

// coverage
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")
