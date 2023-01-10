import sbt.Def.spaceDelimited

organization := "ch.epfl.lara"

ThisBuild / scalaVersion := "3.1.3"
ThisBuild / version := "1.0"
ThisBuild / scalacOptions ++= Seq("-feature", "-language:implicitConversions")
ThisBuild / shellPrompt := (Project.extract(_).currentRef.project + "> ")
ThisBuild / usage := printUsage

ThisBuild / Test / parallelExecution := false
ThisBuild / libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
ThisBuild / testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

// ================================================================================================
// ====================================== COMMON PARAMETERS =======================================
// ================================================================================================

lazy val amyc_entrypoint = "amyc.compiler"
lazy val amy_entrypoint  = "amyc.runner"
lazy val repl_entrypoint = "amyc.repl.repl"

def printUsage =
  println {
    """
      | =================== Amy Programming Language Project ===================
      | Use the following commands to use Amy :
      |     amy  : Compile and start an interpreter for Amy
      |         usage : amy  [PATH_TO_FILES]
      |     amyc : Compile files into WebAssembly
      |         usage : amyc [PATH_TO_FILES]
      |     repl : Start a new REPL session
      |         usage : repl
      |     help : Print into the terminal the given usage
      |         usage : help
      |""".stripMargin
  }


// ================================================================================================
// ============================================ KEYS ==============================================
// ================================================================================================

lazy val amyc = inputKey[Unit]("run amy's compiler")
lazy val amy  = inputKey[Unit]("run amy's interpreter")
lazy val repl = inputKey[Unit]("start a new repl session")
lazy val usage = taskKey[Unit]("print the usage")

// ================================================================================================
// ======================================== SETTINGS ==============================================
// ================================================================================================

lazy val amyc_setting =
  amyc := Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed.toList
    (`amy-compiler` / Compile / run).toTask(args.mkString(" ", " ", " "))
  }.evaluated

lazy val amy_setting =
  amy := Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed.toList
    (`amy-interpreter` / Compile / run).toTask(args.mkString(" ", " ", " "))
  }.evaluated

lazy val amy_repl =
  repl := Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed.toList
    (`amy-repl` / Compile / run).toTask(args.mkString(" ", " ", " "))
  }.evaluated

// ================================================================================================
// ====================================== AMY'S PROJECT ===========================================
// ================================================================================================

lazy val `amy-language` = (project in file("."))
  .aggregate(`amy-compiler`, `amy-interpreter`, `amy-stdlib`, `amy-repl`)
  .enablePlugins(JavaAppPackaging)
  .settings(
    //dockerBaseImage := "openjdk:jre-alpine",
    dockerExposedPorts ++= Seq(),
    amy_setting,
    amyc_setting,
    amy_repl,
    Compile / run := usage.value
  )

// ================================================================================================
// ================================== AMY'S COMPILER PROJECT ======================================
// ================================================================================================

lazy val `amy-compiler` = (project in file("compiler"))
  .settings(
    Compile / mainClass := Some(amyc_entrypoint)
  )

// ================================================================================================
// ==================================== AMY'S INTERPRETER PROJECT =================================
// ================================================================================================
lazy val `amy-interpreter` = (project in file("interpreter"))
  .dependsOn(`amy-compiler` % "compile->compile;test->test")
  .settings(
    Compile / mainClass := Some(amy_entrypoint),
  )

// ================================================================================================
// =================================== AMY'S STANDARD LIBRARY =====================================
// ================================================================================================

lazy val `amy-stdlib` = (project in file("library"))
  .settings(

  )

lazy val `amy-repl` = (project in file("repl"))
  .dependsOn(
    `amy-compiler` % "compile->compile;test->test",
    `amy-interpreter` % "compile->compile;test->test"
  ).settings(
    Compile / mainClass := Some(repl_entrypoint),
    libraryDependencies ++= Seq(
      "org.jline" % "jline-reader" % "3.21.0",
      "org.jline" % "jline-terminal" % "3.21.0",
      "org.jline" % "jline-terminal-jna" % "3.21.0"
    )
  )
