name := "chisel-crv"

organization := "parzival3"

version := "0.1.5"

scalaVersion := "2.12.10"

ThisBuild / libraryDependencies ++= Seq(
  "org.jacop" % "jacop" % "4.7.0",
  "org.scalatest" %% "scalatest" % "3.0.8",
  "org.choco-solver" % "choco-solver" % "4.10.5"
)
githubOwner := "parzival3"
githubRepository := "chisel-crv"
coverageEnabled := true

