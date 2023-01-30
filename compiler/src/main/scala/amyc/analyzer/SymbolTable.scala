package amyc.analyzer

import amyc.ast.SymbolicTreeModule.TypeTree
import amyc.core.Identifier
import amyc.utils.UniqueCounter
import amyc.core.Signatures.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.HashMap

// A class that represents a dictionary of symbols for an Amy program
class SymbolTable {
  private val defsByName = mutable.HashMap[(String, String), Identifier]()
  private val modules = mutable.HashMap[String, Identifier]()

  private val types = mutable.HashMap[Identifier, Identifier]()
  private val functions = mutable.HashMap[Identifier, FunSig]()
  private val constructors = mutable.HashMap[Identifier, ConstrSig]()

  private val typesToConstructors = mutable.HashMap[Identifier, List[Identifier]]()

  private val constrIndexes = new UniqueCounter[Identifier]
  private val funIndexes = new AtomicInteger

  def addModule(name: String): Identifier = {
    val s = Identifier.fresh(name)
    modules += name -> s
    s
  }
  def getModule(name: String): Option[Identifier] = modules.get(name)

  def addType(owner: String, name: String): Identifier = {
    val s = Identifier.fresh(name)
    defsByName += (owner, name) -> s
    types += (s -> modules.getOrElse(owner, sys.error(s"Module $name not found!")))
    s
  }
  def getType(owner: String, name: String): Option[Identifier] =
    defsByName.get(owner,name) filter types.contains
  def getType(symbol: Identifier): Option[Identifier] = types.get(symbol)

  def addConstructor(owner: String, name: String, argTypes: List[TypeTree], parent: Identifier): Identifier = {
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
  def getConstructor(symbol: Identifier): Option[ConstrSig] = constructors.get(symbol)

  def getConstructorsForType(t: Identifier): Option[List[Identifier]] = typesToConstructors.get(t)

  def addFunction(owner: String, name: String, argTypes: List[TypeTree], retType: TypeTree): Identifier = {
    val s = Identifier.fresh(name)
    val idx = funIndexes.incrementAndGet()
    defsByName += (owner, name) -> s
    functions += s -> FunSig(argTypes, retType, getModule(owner).getOrElse(sys.error(s"Module $owner not found!")), idx)
    s
  }
  def getFunction(owner: String, name: String): Option[(Identifier, ApplicationSig[TypeTree])] = {
    for {
      sym <- defsByName.get(owner, name)
      sig <- functions.get(sym)
    } yield (sym, sig)
  }
  def getFunction(symbol: Identifier): Option[FunSig] = functions.get(symbol)

}
