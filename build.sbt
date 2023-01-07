import sbt.Def.spaceDelimited

organization := "ch.epfl.lara"

ThisBuild / scalaVersion := "3.1.3"
ThisBuild / version := "1.0"
ThisBuild / scalacOptions ++= Seq("-feature", "-language:implicitConversions")
ThisBuild / shellPrompt := (Project.extract(_).currentRef.project + "> ")


// ================================================================================================
// ============================================ KEYS ==============================================
// ================================================================================================

lazy val amyc = inputKey[Unit]("run amy's compiler")
lazy val amy  = inputKey[Unit]("run amy's interpreter")
lazy val repl = inputKey[Unit]("start a new repl session")

// ================================================================================================
// ======================================== SETTINGS ==============================================
// ================================================================================================

lazy val amyc_setting =
  amyc := Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed.toList
    val main = "amyc.compiler";
    (`amy-compiler` / Compile / runMain).toTask((main :: args).mkString(" ", " ", " "))
  }.evaluated

lazy val amy_setting =
  amy := Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed.toList
    val main = "amyc.runner";
    (`amy-compiler` / Compile / runMain).toTask((main :: args).mkString(" ", " ", " "))
  }.evaluated

lazy val amy_repl =
  repl := Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed.toList
    val main = "amyc.repl.repl";
    (`amy-repl` / Compile / runMain).toTask((main :: args).mkString(" ", " ", " "))
  }.evaluated

// ================================================================================================
// ====================================== PROJECTS ================================================
// ================================================================================================

lazy val `amy-language` = (project in file("."))
  .aggregate(
    `amy-compiler`,
    `amy-stdlib`,
    `amy-repl`
  )
  .settings(
    amy_setting,
    amyc_setting,
    amy_repl
  )

lazy val `amy-compiler` = (project in file("amyc"))
  .settings(
    Test / parallelExecution := false,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
  )

lazy val `amy-stdlib` = (project in file("library"))
  .settings(

  )

lazy val `amy-repl` = (project in file("repl"))
  .dependsOn(`amy-compiler`)
  .settings(
    libraryDependencies ++= Seq(
      "org.jline" % "jline-reader" % "3.21.0",
      "org.jline" % "jline-terminal" % "3.21.0",
      "org.jline" % "jline-terminal-jna" % "3.21.0"
    )
  )
