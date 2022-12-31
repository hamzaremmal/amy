package amyc.core

import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.{Type, ClassType}

import scala.collection.immutable.List

object Signatures{

  trait Signature[RT <: Type] {
    val argTypes: List[Type]
    val retType: RT
  }

  trait ApplicationSig[RT <: Type](val owner: Identifier) extends Signature[RT]

  /**
    * The signature of a function in the symbol table
    *
    * @param argTypes Types of the args of the function, in order
    * @param retType  Return type of the function
    * @param owner    Name of the module in which the function is defined
    */
  case class FunSig(argTypes: List[Type], retType: Type, override val owner: Identifier) extends ApplicationSig[Type](owner)

  /**
    * The signature of a constructor in the symbol table
    *
    * @param argTypes Types of the args of the constructor, in order
    * @param parent   Identifier of the abstract class that the constructor extends
    * @param index    Constructors extending a parent are numbered, starting at 0 for each parent.
    *                 This is useful for code generation, where we need a runtime representation of which
    *                 instance of the parent type a value represents.
    */
  case class ConstrSig(argTypes: List[Type], parent: Identifier, index: Int) extends Signature[ClassType] {
    val retType = ClassType(parent)
  }

  /**
    * The signature of a lambda in the symbol table
    *
    * @param argTypes
    * @param retType
    * @param index - index in the webassembly table
    */
  case class LambdaSig(argTypes: List[Type], retType: Type, index: Int, override val owner: Identifier) extends ApplicationSig[Type](owner)

}