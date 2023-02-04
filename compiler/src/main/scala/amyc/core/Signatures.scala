package amyc.core

import amyc.ast.SymbolicTreeModule.*
import amyc.core.Symbols.*

import scala.collection.immutable.List

object Signatures{

  trait Signature[RT <: TypeTree]() {
    val argTypes: List[TypeTree]
    val retType: RT
  }

  trait ApplicationSig[RT <: TypeTree] extends Signature[RT]{
    val owner : Symbol
    val idx : Int // Index in the table of a module
  }

  /**
    * The signature of a function in the symbol table
    *
    * @param argTypes Types of the args of the function, in order
    * @param retType  Return type of the function
    * @param owner    Name of the module in which the function is defined
    */
  case class FunSig(argTypes: List[TypeTree], retType: TypeTree, owner: ModuleSymbol, idx: Int) extends ApplicationSig[TypeTree]

  /**
    * The signature of a constructor in the symbol table
    *
    * @param argTypes Types of the args of the constructor, in order
    * @param parent   Identifier of the abstract class that the constructor extends
    * @param index    Constructors extending a parent are numbered, starting at 0 for each parent.
    *                 This is useful for code generation, where we need a runtime representation of which
    *                 instance of the parent type a value represents.
    */
  case class ConstrSig(argTypes: List[TypeTree], parent: Symbol, idx: Int) extends Signature[ClassTypeTree] {
    override val retType: ClassTypeTree = ClassTypeTree(parent)
  }

}