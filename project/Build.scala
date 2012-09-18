import sbt._
import Keys._

object MsasBuild extends Build {
    lazy val msas = Project(id = "msas",
                            base = file(".")) aggregate(core, view)

    lazy val core = Project(id = "msas-core",
                           base = file("modules/core"))

    lazy val view = Project(id = "msas-view",
                           base = file("modules/view")) dependsOn(core)

}

