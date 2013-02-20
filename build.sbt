scalaVersion in ThisBuild := "2.10.0"

scalacOptions in ThisBuild += "-language:experimental.macros"


libraryDependencies in ThisBuild ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.0",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)
