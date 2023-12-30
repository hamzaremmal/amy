package amyc
package ast

import NominalTreeModule as N
import core.Context

/**
 * @author Hamza REMMAL (hamza@remmal.net)
 */
trait TreeTraverser:

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  final def traverse(tree: N.Tree)(using Context): Unit =
    tree match
      case tree: N.Program => traverse(tree)
      case tree: N.ModuleDef => traverse(tree)
      case tree: N.FunDef => traverse(tree)
      case tree: N.AbstractClassDef => traverse(tree)
      case tree: N.CaseClassDef => traverse(tree)
      case tree: N.ValParamDef => traverse(tree)
      case tree: N.TypeParamDef => traverse(tree)
      case tree: N.EmptyExpr => traverse(tree)
      case tree: N.Variable => traverse(tree)
      case tree: N.FunRef => traverse(tree)
      case tree: N.IntLiteral => traverse(tree)
      case tree: N.BooleanLiteral => traverse(tree)
      case tree: N.StringLiteral => traverse(tree)
      case tree: N.UnitLiteral => traverse(tree)
      case tree: N.InfixCall => traverse(tree)
      case tree: N.Call => traverse(tree)
      case tree: N.Not => traverse(tree)
      case tree: N.Neg => traverse(tree)
      case tree: N.Sequence => traverse(tree)
      case tree: N.Let => traverse(tree)
      case tree: N.Ite => traverse(tree)
      case tree: N.Match => traverse(tree)
      case tree: N.MatchCase => traverse(tree)
      case tree: N.Error => traverse(tree)
      case tree: N.WildcardPattern => traverse(tree)
      case tree: N.IdPattern => traverse(tree)
      case tree: N.LiteralPattern[_] => traverse(tree)
      case tree: N.CaseClassPattern => traverse(tree)
      case tree: N.ClassTypeTree => traverse(tree)
      case tree: N.TTypeTree => traverse(tree)
      case tree: N.FunctionTypeTree => traverse(tree)
  end traverse

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  final def traverseChildren(tree: N.Tree)(using Context): Unit =
    tree match
      case N.Program(modules) =>
        for module <- modules do traverse(module)
      case N.ModuleDef(_, defs, expr) =>
        for defn <- defs do traverse(defn)
        for expr <- expr do traverse(expr)
      case N.FunDef(_, tparams, vparams, ret, body) =>
        for tparam <- tparams do traverse(tparam)
        for vparam <- vparams do traverse(vparam)
        traverse(ret)
        traverse(body)
      case N.AbstractClassDef(_) => ()
      case N.CaseClassDef(_, fields, _) =>
        for field <- fields do traverse(field)
      case N.ValParamDef(_, tpe) => traverse(tpe)
      case N.TypeParamDef(_) => ()
      case N.EmptyExpr() => ()
      case N.Variable(_) => ()
      case N.FunRef(_) => ()
      case N.IntLiteral(_) => ()
      case N.BooleanLiteral(_) => ()
      case N.StringLiteral(_) => ()
      case N.UnitLiteral() => ()
      case N.InfixCall(lhs, _, rhs) =>
        traverse(lhs)
        traverse(rhs)
      case N.Call(_, targs, vargs) =>
        for targ <- targs do traverse(targ)
        for varg <- vargs do traverse(varg)
      case N.Not(tree) => traverse(tree)
      case N.Neg(tree) => traverse(tree)
      case N.Sequence(e1, e2) =>
        traverse(e1)
        traverse(e2)
      case N.Let(df, rhs, body) =>
        traverse(df)
        traverse(rhs)
        traverse(body)
      case N.Ite(cond, thenn, elze) =>
        traverse(cond)
        traverse(thenn)
        traverse(elze)
      case N.Match(scrut, cases) =>
        traverse(scrut)
        for `case` <- cases do traverse(`case`)
      case N.MatchCase(pat, expr) =>
        traverse(pat)
        traverse(expr)
      case N.Error(tree) => traverse(tree)
      case N.WildcardPattern() => ()
      case N.IdPattern(_) => ()
      case N.LiteralPattern(lit) => traverse(lit)
      case N.CaseClassPattern(_, args) =>
        for arg <- args do traverse(arg)
      case N.ClassTypeTree(_) => ()
      case N.TTypeTree(_) => ()
      case N.FunctionTypeTree(args, ret) =>
        for arg <- args do traverse(arg)
        traverse(ret)
  end traverseChildren

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def traverse(tree: N.Program)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def traverse(tree: N.ModuleDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def traverse(tree : N.FunDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.AbstractClassDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.CaseClassDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.ValParamDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.TypeParamDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.EmptyExpr)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Variable)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.FunRef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.IntLiteral)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.BooleanLiteral)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.StringLiteral)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.UnitLiteral)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.InfixCall)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Call)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Not)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Neg)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Sequence)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Let)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Ite)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Match)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.MatchCase)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.Error)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.WildcardPattern)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.IdPattern)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.LiteralPattern[_])(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.CaseClassPattern)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.ClassTypeTree)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.TTypeTree)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: N.FunctionTypeTree)(using Context): Unit =
    traverseChildren(tree)
  end traverse