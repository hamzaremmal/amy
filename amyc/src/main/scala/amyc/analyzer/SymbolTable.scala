package amyc.analyzer

import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.UniqueCounter

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.HashMap

trait Signature[RT <: Type]{
  val argTypes: List[Type]
  val retType: RT
}

trait ApplicationSig[RT <: Type](val owner: Identifier) extends Signature[RT]

/**
  * The signature of a function in the symbol table
  *
  * @param argTypes Types of the args of the function, in order
  * @param retType Return type of the function
  * @param owner Name of the module in which the function is defined
  */
case class FunSig(argTypes: List[Type], retType: Type, override val owner: Identifier) extends ApplicationSig[Type](owner)

/**
  * The signature of a constructor in the symbol table
  *
  * @param argTypes Types of the args of the constructor, in order
  * @param parent Identifier of the abstract class that the constructor extends
  * @param index Constructors extending a parent are numbered, starting at 0 for each parent.
  *              This is useful for code generation, where we need a runtime representation of which
  *              instance of the parent type a value represents.
  */
case class ConstrSig(argTypes: List[Type], parent: Identifier, index: Int) extends Signature[ClassType]{
  val retType = ClassType(parent)
}

/**
  * The signature of a lambda in the symbol table
  * @param argTypes
  * @param retType
  * @param index - index in the webassembly table
  */
case class LambdaSig(argTypes: List[Type], retType: Type, index: Int, override val owner: Identifier) extends ApplicationSig[Type](owner)

// A class that represents a dictionary of symbols for an Amy program
class SymbolTable {
  private val defsByName = HashMap[(String, String), Identifier]()
  private val modules = HashMap[String, Identifier]()

  private val types = HashMap[Identifier, Identifier]()
  private val functions = HashMap[Identifier, ApplicationSig[Type]]()
  private val constructors = HashMap[Identifier, ConstrSig]()

  private val typesToConstructors = HashMap[Identifier, List[Identifier]]()

  private val constrIndexes = new UniqueCounter[Identifier]
  private val lambdaIndexes = new AtomicInteger

  def addModule(name: String) = {
    val s = Identifier.fresh(name)
    modules += name -> s
    s
  }
  def getModule(name: String) = modules.get(name)

  def addType(owner: String, name: String) = {
    val s = Identifier.fresh(name)
    defsByName += (owner, name) -> s
    types += (s -> modules.getOrElse(owner, sys.error(s"Module $name not found!")))
    s
  }
  def getType(owner: String, name: String) =
    defsByName.get(owner,name) filter types.contains
  def getType(symbol: Identifier) = types.get(symbol)

  def addConstructor(owner: String, name: String, argTypes: List[Type], parent: Identifier) = {
    val s = Identifier.fresh(name)
    defsByName += (owner, name) -> s
    constructors += s -> ConstrSig(
      argTypes,
      parent,
      constrIndexes.next(parent)
    )
    typesToConstructors += parent -> (typesToConstructors.getOrElse(parent, Nil) :+ s)
    s
  }
  def getConstructor(owner: String, name: String): Option[(Identifier, ConstrSig)] = {
    for {
      sym <- defsByName.get(owner, name)
      sig <- constructors.get(sym)
    } yield (sym, sig)
  }
  def getConstructor(symbol: Identifier) = constructors.get(symbol)

  def getConstructorsForType(t: Identifier) = typesToConstructors.get(t)

  def addFunction(owner: String, name: String, argTypes: List[Type], retType: Type) = {
    val s = Identifier.fresh(name)
    defsByName += (owner, name) -> s
    functions += s -> FunSig(argTypes, retType, getModule(owner).getOrElse(sys.error(s"Module $owner not found!")))
    s
  }
  def getFunction(owner: String, name: String): Option[(Identifier, ApplicationSig[Type])] = {
    for {
      sym <- defsByName.get(owner, name)
      sig <- functions.get(sym)
    } yield (sym, sig)
  }
  def getFunction(symbol: Identifier) = functions.get(symbol)

  def addLambda(owner:String, argTypes: List[Type], retType: Type) =
    val idx = lambdaIndexes.incrementAndGet()
    val name = s"$owner$$_$idx"
    val ident = Identifier.fresh(name)
    functions.addOne((ident, LambdaSig(argTypes, retType,idx, getModule(owner).get)))
    ident

}
