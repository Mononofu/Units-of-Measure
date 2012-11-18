import sbt._
import Keys._

object HelloBuild extends Build {
    lazy val root = Project(id = "hello",
                            base = file(".")) dependsOn(macroImpl)

    lazy val macroImpl = Project(id = "hello-foo",
                           base = file("macros"))
}