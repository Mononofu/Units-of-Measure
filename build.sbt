scalaVersion in ThisBuild := "2.10.0"

scalacOptions in ThisBuild += "-language:experimental.macros"


libraryDependencies in ThisBuild ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.0",
  "org.scalatest" % "scalatest_2.10.0" % "2.0.M5" % "test"
)
