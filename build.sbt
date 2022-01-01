ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"

lazy val root = (project in file("."))
  .settings(
    name := "rockthejvm-cats",
    idePackagePrefix := Some("us.elifk")
  )
