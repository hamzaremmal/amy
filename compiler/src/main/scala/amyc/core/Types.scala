package amyc
package core

import amyc.utils.UniqueCounter

/**
  *
  */
object Types :

  /**
    *
    */
  trait Type {

    // Check subtyping
    infix def <:<(lhs: Type): Boolean =
      isBottomType || lhs.isBottomType || this =:= lhs

    // Check the equality of 2 types
    infix def =:=(lhs: Type): Boolean =
      (this, lhs) match
        case (ClassType(i), ClassType(j)) if i == j => true
        case _ => this == lhs

    def isBottomType: Boolean =
      this =:= BottomType

  }

  // To mark the fact that a tree has no type
  // This usually means that the type should be inferred
  case object NoType extends Type

  // This type is used to fill the type information of a tree
  // when an error happens at the typer level
  case object ErrorType extends Type

  case object WildCardType extends Type

  // Bottom type (Should not be defined like this)
  // This type will be removed in the future
  case object BottomType extends Type

  /**
    *
    * @param lhs
    * @param rhs
    */
  case class OrType(lhs : Type, rhs: Type) extends Type

  /**
    *
    * @param args
    * @param rte
    */
  case class FunctionType(args: List[Type], rte: Type) extends Type

  /**
    *
    * @param qname
    */
  case class ClassType(qname: Identifier) extends Type

  case class TypeParameter(name: Identifier) extends Type

  //
  //
  //

  // Represents a type variable.
  // It extends Type, but it is meant only for internal type checker use,
  //  since no Amy value can have such type.
  case class TypeVariable private(id: Int) extends Type

  object TypeVariable {
    private val c = new UniqueCounter[Unit]

    def fresh(): TypeVariable = TypeVariable(c.next(()))
  }

  case class MultiTypeVariable() extends Type {
    private var t: List[Type] = Nil


    override def toString: String =
      t.toString()

    def add(tpe: Type) =
      t ::= tpe

    def resolve(using ctx: Context): Type = {
      def consistentacc(xs: List[Type], acc: Type): Type =
        xs match
          case TypeVariable(_) :: ys => consistentacc(ys, acc)
          case y :: ys if y <:< acc => consistentacc(ys, y)
          case y :: ys => consistentacc(ys, OrType(acc, y))
          case Nil => acc

      val inferred = consistentacc(t, BottomType)
      for case tv@TypeVariable(_) <- t do
        ctx.tv.update(tv, inferred)
      inferred
    }

    def bind(using ctx: Context): MultiTypeVariable =
      def bindt(tpe: Type): Option[Type] =
        tpe match
          case tv@TypeVariable(_) =>
            ctx.tv.get(tpe).flatMap(bindt).orElse(Some(tv))
          case t@MultiTypeVariable() =>
            Some(t.bind.resolve)
          case _ => Some(tpe)

      t = for
        tp <- t
        b <- bindt(tp)
      yield b
      this

  }