import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:existentials"),
    scalaVersion := "2.11.0-SNAPSHOT",
    scalaOrganization := "org.scala-lang.macro-paradise",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("samples"),
    settings = buildSettings
  ) aggregate(macros, samples)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-reflect" % _))
  )

  lazy val units: Project = Project(
    "units",
    file("units"),
    settings = buildSettings
  ) dependsOn(macros)

  lazy val samples: Project = Project(
    "samples",
    file("samples"),
    settings = buildSettings
  ) dependsOn(units)
}
