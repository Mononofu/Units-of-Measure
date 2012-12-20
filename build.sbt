scalaVersion in ThisBuild := "2.10.0-RC5"

scalacOptions in ThisBuild += "-language:experimental.macros"


libraryDependencies in ThisBuild ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.0-RC5",
  "org.scalatest" % "scalatest_2.10.0-RC5" % "2.0.M5-B1" % "test"
)
