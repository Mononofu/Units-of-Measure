scalaVersion in ThisBuild := "2.10.0-RC2"

scalacOptions += "-language:experimental.macros"


libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.0-RC1"
)
