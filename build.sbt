import sbt.Def.spaceDelimited

import com.github.sbt.jacoco.JacocoKeys.jacocoAggregateReportSettings

ThisBuild / scalaVersion := "3.3.0"
ThisBuild / version := "0.0.1"
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

def printUsage =
  println {
    """
      | =================== Amy Programming Language Project ===================
      | Use the following commands to use Amy :
      |     amy  : Compile and start an interpreter for Amy
      |         usage : amy  [PATH_TO_FILES]
      |     amyc : Compile files into WebAssembly
      |         usage : amyc [PATH_TO_FILES]
      |     help : Print into the terminal the given usage
      |         usage : help
      |""".stripMargin
  }


// ================================================================================================
// ============================================ KEYS ==============================================
// ================================================================================================

lazy val amyc = inputKey[Unit]("run amy's compiler")
lazy val amy  = inputKey[Unit]("run amy's interpreter")
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

// ================================================================================================
// ====================================== AMY'S PROJECT ===========================================
// ================================================================================================

lazy val `amy-language` = (project in file("."))
  .aggregate(`amy-compiler`, `amy-interpreter`, `amy-stdlib`)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    //dockerBaseImage := "openjdk:jre-alpine",
    dockerExposedPorts ++= Seq(),
    amy_setting,
    amyc_setting,
    Compile / run := usage.value,
    Test / jacocoAggregateReportSettings := JacocoReportSettings(
      "Jacoco Aggregate Coverage Report",
      None,
      JacocoThresholds(0.0,0.0,0.0,0.0,0.0,0.0),
      Seq(JacocoReportFormats.ScalaHTML, JacocoReportFormats.XML),
      "utf-8")
  )

// ================================================================================================
// ================================== AMY'S COMPILER PROJECT ======================================
// ================================================================================================

lazy val `amy-compiler` = (project in file("compiler"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    Compile / mainClass := Some(amyc_entrypoint),
  )

// ================================================================================================
// ==================================== AMY'S INTERPRETER PROJECT =================================
// ================================================================================================
lazy val `amy-interpreter` = (project in file("interpreter"))
  .dependsOn(`amy-compiler` % "compile->compile;test->test")
  .enablePlugins(JavaAppPackaging)
  .settings(
    Compile / mainClass := Some(amy_entrypoint),
  )

// ================================================================================================
// =================================== AMY'S STANDARD LIBRARY =====================================
// ================================================================================================

lazy val `amy-stdlib` = project in file("library")
