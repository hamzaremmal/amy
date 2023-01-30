package amyc.analyzer

import amyc.ast.Identifier
import amyc.core.Context
import amyc.reporter

/* alias to write Scope definition */
private type Bag = Map[String, Identifier]

/**
  *
  * @param parent
  * @param params
  * @param locals
  */
sealed case class Scope protected (parent: Option[Scope], params : Bag, locals : Bag) :

  /**
    * Create a new Scope with the mapping (name -> id) in locals
    * The parent of the new Scope is the caller
    * @param str (name) -
    * @param id (Identifier) -
    * @return
    */
  final def withLocal(name : String, id : Identifier) : Scope =
    Scope(Some(this), params, locals + (name -> id))

  /**
    *
    * @param locals
    * @return
    */
  final def withLocals(locals : Bag) : Scope =
    Scope(Some(this), params, locals)

  /**
    * Create a new Scope with the mapping (name -> id) in params
    * The parent of the new Scope is the caller
    * @param name
    * @param id
    * @return
    */
  final def withParam(name : String, id : Identifier) : Scope =
    Scope(Some(this), params + (name -> id), locals)

  /**
    *
    * @param params
    * @return
    */
  final def withParams(params : Bag): Scope =
    Scope(Some(this), params, locals)

  /**
    *
    * @param name
    * @return
    */
  def isLocal(name: String) : Boolean =
    locals.contains(name) || parent.map(_.isLocal(name)).get

  /**
    *
    * @param name
    * @return
    */
  def isParam(name: String): Boolean =
    params.contains(name) || parent.map(_.isParam(name)).get

  /**
    * Resolve a new in the current Scope
    * The Search is recursive !!
    * @param name
    * @param Context
    * @return
    */
  def resolve(name : String) : Option[Identifier] =
    resolveInScope(name) orElse parent.flatMap(_.resolve(name))

  /**
    * Resolve a new in the current Scope
    * The Search is not recursive !!
    * @param name
    * @return
    */
  def resolveInScope(name : String) : Option[Identifier] =
    // Local variables shadow parameters!
    locals.get(name) orElse params.get(name)


/**
  *
  */
object Scope :

  /**
    * Create a fresh Scope
    * @return
    */
  def fresh: Scope = EmptyScope

  /**
    *
    * @param lhs
    * @param rhs
    * @param parent
    * @return
    */
  def combine(lhs : Scope, rhs : Scope)(using parent : Scope) : Scope =
    Scope(Some(parent), lhs.params ++ rhs.params, lhs.locals ++ rhs.locals)


/**
  * Empty Scope
  */
object EmptyScope extends Scope(None, Map.empty, Map.empty):

  /* Override to avoid Exception in Scope::isLocal */
  override def isLocal(name: String): Boolean =
    false

  /* Override to avoid Exception in Scope::isParam */
  override def isParam(name: String): Boolean =
    false


