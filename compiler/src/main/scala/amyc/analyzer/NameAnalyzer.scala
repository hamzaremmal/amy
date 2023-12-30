package amyc
package analyzer

import Transformer.*
import ast.{NominalTreeModule as N, SymbolicTreeModule as S}
import core.*
import utils.*

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
    for mod <- p.modules do checkModuleConsistency(mod)

    // Step 3: Discover types
    for mod <- p.modules do registerTypes(mod)

    // Step 4: Discover type constructors
    for m <- p.modules do registerConstructors(m)

    // Step 5: Discover functions signatures.
    for m <- p.modules do registerFunctions(m)

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    transformProgram(p)

  }

  // ==============================================================================================
  // ===================================== REGISTER FUCTIONS ======================================
  // ==============================================================================================

  /**
    * @param mod
    * @param Context
    */
  def registerFunctions(mod: N.ModuleDef)(using Context) =
    val owner = symbols.module(mod.name)
    for case fd @ N.FunDef(name, tparams, vparams, tpe, _) <- mod.defs do
      symbols.addFunction(owner, name, fd.mods, tparams, vparams, tpe)

  /**
    * @param mod
    * @param Context
    */
  def registerConstructors(mod: N.ModuleDef)(using Context) =
    for case N.CaseClassDef(name, fields, parent) <- mod.defs do
      symbols.addConstructor(
        mod.name,
        name,
        fields,
        symbols.`type`(mod.name, parent)
      )

  /**
    * @param prog
    * @param Context
    */
  def registerModules(prog: N.Program)(using Context) =
    val modNames = prog.modules.groupBy(_.name)
    for(name, modules) <- modNames do
      if modules.size > 1 then
        reporter.fatal(
          s"Two modules named $name in program",
          modules.head.position
        )
    for mod <- modNames.keys.toList do
      val id = symbols.addModule(mod)
      ctx.withScope(id)

  /**
    * @param mod
    * @param Context
    */
  def registerTypes(mod: N.ModuleDef)(using Context) =
    for case N.AbstractClassDef(name) <- mod.defs do symbols.addType(mod.name, name)

  /**
    * @param mod
    * @param Context
    */
  def checkModuleConsistency(mod: N.ModuleDef)(using Context) =
    val names = mod.defs.groupBy(_.name)
    for(name, defs) <- names do
      if(defs.size > 1) {
        reporter.fatal(
          s"Two definitions named $name in module ${mod.name}",
          defs.head
        )
      }

}
