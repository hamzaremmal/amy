course := "epfl"
assignment := "amyc"
scalaVersion := "3.1.3"

version := "1.7"
organization := "ch.epfl.lara"

scalacOptions ++= Seq("-feature")

Test / parallelExecution := false
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
