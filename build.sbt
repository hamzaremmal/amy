import sbt.Def.spaceDelimited

organization := "ch.epfl.lara"

// ================================================================================================
// ============================================ KEYS ==============================================
// ================================================================================================

lazy val amyc = inputKey[Unit]("run amy's compiler")
lazy val amy  = inputKey[Unit]("run amy's interpreter")

// ================================================================================================
// ====================================== PROJECTS ================================================
// ================================================================================================

lazy val `amy-language` = (project in file(".")).settings(
  version := "1.7",
  scalaVersion := "3.1.3",
  amy := Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed.toList
    val main = "amyc.Lab1";
    (`amy-compiler` / Compile / runMain).toTask((main :: args).mkString(" ", " ", " "))
  }.evaluated,
  amyc := Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed.toList
    val main = "amyc.Lab5";
    (`amy-compiler` / Compile / runMain).toTask((main :: args).mkString(" ", " ", " "))
  }.evaluated
)

lazy val `amy-compiler` = (project in file("amyc")).settings(
  scalaVersion := "3.1.3",
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  organization := "ch.epfl.lara",
  Test / parallelExecution := false,
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
)

lazy val `amy-stdlib` = project in file("library")
