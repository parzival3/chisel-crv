name := "chisel-crv"

organization := "parzival3"

version := "0.2.2"

scalaVersion := "2.12.10"

ThisBuild / libraryDependencies ++= Seq(
  "org.jacop" % "jacop" % "4.7.0",
  "org.scalatest" %% "scalatest" % "3.0.8",
  "org.choco-solver" % "choco-solver" % "4.10.5"
)

coverageEnabled := true

