package amyc
package ast

import SymbolicTreeModule as S
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
  final def traverse(tree: S.Tree)(using Context): Unit =
    tree match
      case tree: S.Program => traverse(tree)
      case tree: S.ModuleDef => traverse(tree)
      case tree: S.FunDef => traverse(tree)
      case tree: S.AbstractClassDef => traverse(tree)
      case tree: S.CaseClassDef => traverse(tree)
      case tree: S.ValParamDef => traverse(tree)
      case tree: S.TypeParamDef => traverse(tree)
      case tree: S.EmptyExpr => traverse(tree)
      case tree: S.Variable => traverse(tree)
      case tree: S.FunRef => traverse(tree)
      case tree: S.IntLiteral => traverse(tree)
      case tree: S.BooleanLiteral => traverse(tree)
      case tree: S.StringLiteral => traverse(tree)
      case tree: S.UnitLiteral => traverse(tree)
      case tree: S.InfixCall => traverse(tree)
      case tree: S.Call => traverse(tree)
      case tree: S.Not => traverse(tree)
      case tree: S.Neg => traverse(tree)
      case tree: S.Sequence => traverse(tree)
      case tree: S.Let => traverse(tree)
      case tree: S.Ite => traverse(tree)
      case tree: S.Match => traverse(tree)
      case tree: S.MatchCase => traverse(tree)
      case tree: S.Error => traverse(tree)
      case tree: S.WildcardPattern => traverse(tree)
      case tree: S.IdPattern => traverse(tree)
      case tree: S.LiteralPattern[_] => traverse(tree)
      case tree: S.CaseClassPattern => traverse(tree)
      case tree: S.ClassTypeTree => traverse(tree)
      case tree: S.TTypeTree => traverse(tree)
      case tree: S.FunctionTypeTree => traverse(tree)
  end traverse

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  final def traverseChildren(tree: S.Tree)(using Context): Unit =
    tree match
      case S.Program(modules) =>
        for module <- modules do traverse(module)
      case S.ModuleDef(_, defs, expr) =>
        for defn <- defs do traverse(defn)
        for expr <- expr do traverse(expr)
      case S.FunDef(_, tparams, vparams, ret, body) =>
        for tparam <- tparams do traverse(tparam)
        for vparam <- vparams do traverse(vparam)
        traverse(ret)
        traverse(body)
      case S.AbstractClassDef(_) => ()
      case S.CaseClassDef(_, fields, _) =>
        for field <- fields do traverse(field)
      case S.ValParamDef(_, tpe) => traverse(tpe)
      case S.TypeParamDef(_) => ()
      case S.EmptyExpr() => ()
      case S.Variable(_) => ()
      case S.FunRef(_) => ()
      case S.IntLiteral(_) => ()
      case S.BooleanLiteral(_) => ()
      case S.StringLiteral(_) => ()
      case S.UnitLiteral() => ()
      case S.InfixCall(lhs, _, rhs) =>
        traverse(lhs)
        traverse(rhs)
      case S.Call(_, targs, vargs) =>
        for targ <- targs do traverse(targ)
        for varg <- vargs do traverse(varg)
      case S.Not(tree) => traverse(tree)
      case S.Neg(tree) => traverse(tree)
      case S.Sequence(e1, e2) =>
        traverse(e1)
        traverse(e2)
      case S.Let(df, rhs, body) =>
        traverse(df)
        traverse(rhs)
        traverse(body)
      case S.Ite(cond, thenn, elze) =>
        traverse(cond)
        traverse(thenn)
        traverse(elze)
      case S.Match(scrut, cases) =>
        traverse(scrut)
        for `case` <- cases do traverse(`case`)
      case S.MatchCase(pat, expr) =>
        traverse(pat)
        traverse(expr)
      case S.Error(tree) => traverse(tree)
      case S.WildcardPattern() => ()
      case S.IdPattern(_) => ()
      case S.LiteralPattern(lit) => traverse(lit)
      case S.CaseClassPattern(_, args) =>
        for arg <- args do traverse(arg)
      case S.ClassTypeTree(_) => ()
      case S.TTypeTree(_) => ()
      case S.FunctionTypeTree(args, ret) =>
        for arg <- args do traverse(arg)
        traverse(ret)
  end traverseChildren

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def traverse(tree: S.Program)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def traverse(tree: S.ModuleDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def traverse(tree : S.FunDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.AbstractClassDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.CaseClassDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.ValParamDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.TypeParamDef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.EmptyExpr)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Variable)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.FunRef)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.IntLiteral)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.BooleanLiteral)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.StringLiteral)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.UnitLiteral)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.InfixCall)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Call)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Not)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Neg)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Sequence)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Let)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Ite)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Match)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.MatchCase)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.Error)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.WildcardPattern)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.IdPattern)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.LiteralPattern[_])(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.CaseClassPattern)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.ClassTypeTree)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.TTypeTree)(using Context): Unit =
    traverseChildren(tree)
  end traverse

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def traverse(tree: S.FunctionTypeTree)(using Context): Unit =
    traverseChildren(tree)
  end traverse