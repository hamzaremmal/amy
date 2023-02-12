package amyc.core

import amyc.ast.SymbolicTreeModule.*
import amyc.core.Symbols.*

import scala.collection.immutable.List

object Signatures{

  trait Signature[RT <: TypeTree]() {
    val argTypes: List[TypeTree]
    val retType: RT
  }

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