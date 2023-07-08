ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "axidraw",
    idePackagePrefix := Some("com.axidraw"),
    libraryDependencies ++= Seq(
      "com.fazecast" % "jSerialComm" % "[2.0.0,3.0.0)",
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-effect" % "3.3.1",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.16" % Test
    )
  )
