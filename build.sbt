ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "CheckOut2.0"
  )


libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.6"