object CommonResolvers {
  import sbt._
  import sbt.Keys._

  def settings: Setting[_] = resolvers ++= Seq(
    "Maven.org" at "http://repo1.maven.org/maven2"
    , "Sun Maven2 Repo" at "http://download.java.net/maven/2"
    , "Scala-Tools" at "http://scala-tools.org/repo-releases/"
    , "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish"
    , "Oracle Maven2 Repo" at "http://download.oracle.com/maven"
    , "spy" at "http://files.couchbase.com/maven2/"
    , "Twitter" at "http://maven.twttr.com/"
    , "Akka Repository" at "http://repo.akka.io/releases/"
    , "Cloudera Repository" at "https://repository.cloudera.com/artifactory/cloudera-repos/"
    , "MVN Repo" at "http://mvnrepository.com/artifact"
    , Resolver.sonatypeRepo("releases")
    , Resolver.sonatypeRepo("snapshots")
    , Resolver.typesafeRepo("releases")
    , Resolver.jcenterRepo
    // the below may need to be added to ~/.sbt/0.13/global.sbt
    // unfortunately there isn't a good way to add it to the repo
    , "Artima Maven Repository" at "http://repo.artima.com/releases"
  )
}
