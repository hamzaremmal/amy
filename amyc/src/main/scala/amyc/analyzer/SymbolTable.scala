package amyc.analyzer

import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.UniqueCounter
import amyc.core.Signatures.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.HashMap

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
