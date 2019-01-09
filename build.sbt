name := "twiddle"

scalaVersion := "2.12.4"
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2"
libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"

scalacOptions += "-deprecation"
scalacOptions += "-feature"
// scalacOptions += "-Xlog-implicits"