package amyc
package analyzer

import amyc.*
import amyc.core.*
import amyc.utils.*
import amyc.ast.{Identifier, NominalTreeModule as N, SymbolicTreeModule as S}
import amyc.analyzer.Transformer.*
import amyc.core.Types.ClassType

// Name analyzer for Amy
// Takes a nominal program (names are plain string, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates symbol table.
object NameAnalyzer extends Pipeline[N.Program, S.Program] {

  override val name = "NameAnalyzer"
  override def run(p: N.Program)(using Context): S.Program = {

    // Step 0: Initialize symbol table
    ctx.withSymTable(new SymbolTable)

    // Step 1: Add modules
    registerModules(p)

    // Step 2: Check name uniqueness in modules
    for mod <- p.modules do
      checkModuleConsistency(mod)

    // Step 3: Discover types
    for mod <- p.modules do
      registerTypes(mod)

    // Step 4: Discover type constructors
    for m <- p.modules do
      registerConstructors(m)

    // Step 5: Discover functions signatures.
    for m <- p.modules do
      registerFunctions(m)

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    transformProgram(p)

  }

  // ==============================================================================================
  // ===================================== REGISTER FUCTIONS ======================================
  // ==============================================================================================

  def registerFunctions(mod: N.ModuleDef)(using Context) =
    for N.FunDef(name, params, retType1, _) <- mod.defs do
      val argTypes = params map (p => transformType(p.tt, mod.name))
      val retType2 = transformType(retType1, mod.name)
      symbols.addFunction(mod.name, name, argTypes, retType2)

  def registerConstructors(mod: N.ModuleDef)(using Context) =
    for cc@N.CaseClassDef(name, fields, parent) <- mod.defs do
      val argTypes = fields map (tt => transformType(tt, mod.name))
      val retType = symbols.getType(mod.name, parent).getOrElse {
        reporter.fatal(s"Parent class $parent not found", cc)
      }
      symbols.addConstructor(mod.name, name, argTypes, retType)

  def registerModules(prog: N.Program)(using Context) =
    val modNames = prog.modules.groupBy(_.name)
    for (name, modules) <- modNames do
      if (modules.size > 1) {
        reporter.fatal(s"Two modules named $name in program", modules.head.position)
      }
    for mod <- modNames.keys.toList do
      symbols.addModule(mod)

  def registerTypes(mod: N.ModuleDef)(using Context) =
     for N.AbstractClassDef(name) <- mod.defs do
       val id = symbols.addType(mod.name, name)
       ctx._types += (id -> ClassType(id))

  def checkModuleConsistency(mod: N.ModuleDef)(using Context) =
     val names = mod.defs.groupBy(_.name)
     for (name, defs) <- names do
      if (defs.size > 1) {
         reporter.fatal(s"Two definitions named $name in module ${mod.name}", defs.head)
      }

}
