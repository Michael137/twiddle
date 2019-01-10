name := "twiddle"

scalaVersion := "2.12.4"
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

scalacOptions += "-deprecation"
scalacOptions += "-feature"
// scalacOptions += "-Xlog-implicits"