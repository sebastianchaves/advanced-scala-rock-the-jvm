ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "advanced-scala-rock-the-jvm",
    idePackagePrefix := Some("com.sebastianchaves.courses")
  )
