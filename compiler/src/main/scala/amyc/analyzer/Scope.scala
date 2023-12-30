package amyc.analyzer

import amyc.core.Symbols.*

/* alias to write Scope definition */
private type Bag = Map[String, Symbol]

/**
  *
  * @param parent
  * @param vparams
  * @param locals
  */
sealed case class Scope protected (parent: Option[Scope], tparams: Bag, vparams: Bag, locals: Bag) :

  /**
    * Create a new Scope with the mapping (name -> id) in locals
    * The parent of the new Scope is the caller
    * @param str (name) -
    * @param id (Identifier) -
    * @return
    */
  final def withLocal(name : String, id : Symbol) : Scope =
    Scope(Some(this), tparams, vparams, locals + (name -> id))

  /**
    *
    * @param locals
    * @return
    */
  final def withLocals(locals: Bag): Scope =
    Scope(Some(this), tparams, vparams, locals)

  /**
    * Create a new Scope with the mapping (name -> id) in params
    * The parent of the new Scope is the caller
    * @param name
    * @param id
    * @return
    */
  final def withVParam(name : String, id : Symbol) : Scope =
    Scope(Some(this), tparams, vparams + (name -> id), locals)

  /**
    *
    * @param params
    * @return
    */
  final def withVParams(params : Bag): Scope =
    Scope(Some(this), tparams, params, locals)

  /**
   * Create a new Scope with the mapping (name -> id) in params
   * The parent of the new Scope is the caller
   *
   * @param name
   * @param id
   * @return
   */
  final def withTParam(name: String, id: Symbol): Scope =
    Scope(Some(this), tparams + (name -> id), vparams, locals)

  /**
   *
   * @param params
   * @return
   */
  final def withTParams(tparams: Bag): Scope =
    Scope(Some(this), tparams, vparams, locals)

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
    vparams.contains(name) || parent.map(_.isParam(name)).get

  /**
    * Resolve a new in the current Scope
    * The Search is recursive !!
    * @param name
    * @param Context
    * @return
    */
  def resolve(name : String) : Option[Symbol] =
    resolveInScope(name) orElse parent.flatMap(_.resolve(name))

  /**
    * Resolve a new in the current Scope
    * The Search is not recursive !!
    * @param name
    * @return
    */
  def resolveInScope(name : String) : Option[Symbol] =
    // Local variables shadow parameters!
    locals.get(name) orElse vparams.get(name)


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
    Scope(Some(parent), lhs.tparams ++ rhs.tparams, lhs.vparams ++ rhs.vparams, lhs.locals ++ rhs.locals)


/**
  * Empty Scope
  */
object EmptyScope extends Scope(None, Map.empty, Map.empty, Map.empty):

  /* Override to avoid Exception in Scope::isLocal */
  override def isLocal(name: String): Boolean =
    false

  /* Override to avoid Exception in Scope::isParam */
  override def isParam(name: String): Boolean =
    false


