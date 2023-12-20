ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

val catsVersion = "3.5.2"
val http4sVersion = "0.23.24"

libraryDependencies += "org.typelevel" %% "cats-effect" % catsVersion
libraryDependencies += "org.http4s" %% "http4s-ember-client" % http4sVersion
libraryDependencies += "org.typelevel" %% "cats-parse" % "1.0.0"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)
enablePlugins(NativeImagePlugin)

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-4s",
    idePackagePrefix := Some("com.kaaveland.aoc"),
    Compile / run / fork := true,
    nativeImageOptions += "--no-fallback",
    nativeImageVersion := "22.1.0"
  )
