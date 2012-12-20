import sbt._
import Keys._

object HelloBuild extends Build {
    lazy val root = Project(id = "macros",
                            base = file(".")) dependsOn(macroImpl)

    lazy val macroImpl = Project(id = "macroimpl",
                           base = file("macros"))
}
