package amyc.utils

// Contains a reporter and configuration for the compiler
case class Context(
  reporter: Reporter,
  files: List[String],
  printTokens: Boolean = false,
  printTrees: Boolean = false,
  printNames: Boolean = false,
  interpret: Boolean = false,
  help: Boolean = false
)
