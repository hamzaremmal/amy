package amyc
package transform

import ast.NominalTreeModule as N
import core.Context

/**
 * @author Hamza REMMAL (hamza@remmal.net)
 */
trait TreeTransformer:

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  final def transform(tree: N.Tree)(using Context): tree.type =
    tree match
      case tree: N.Program => transform(tree).asInstanceOf
      case tree: N.ModuleDef => transform(tree).asInstanceOf
      case tree: N.FunDef => transform(tree).asInstanceOf
      case tree: N.AbstractClassDef => transform(tree).asInstanceOf
      case tree: N.CaseClassDef => transform(tree).asInstanceOf
      case tree: N.ValParamDef => transform(tree).asInstanceOf
      case tree: N.TypeParamDef => transform(tree).asInstanceOf
      case tree: N.EmptyExpr => transform(tree).asInstanceOf
      case tree: N.Variable => transform(tree).asInstanceOf
      case tree: N.FunRef => transform(tree).asInstanceOf
      case tree: N.IntLiteral => transform(tree).asInstanceOf
      case tree: N.BooleanLiteral => transform(tree).asInstanceOf
      case tree: N.StringLiteral => transform(tree).asInstanceOf
      case tree: N.UnitLiteral => transform(tree).asInstanceOf
      case tree: N.InfixCall => transform(tree).asInstanceOf
      case tree: N.Call => transform(tree).asInstanceOf
      case tree: N.Not => transform(tree).asInstanceOf
      case tree: N.Neg => transform(tree).asInstanceOf
      case tree: N.Sequence => transform(tree).asInstanceOf
      case tree: N.Let => transform(tree).asInstanceOf
      case tree: N.Ite => transform(tree).asInstanceOf
      case tree: N.Match => transform(tree).asInstanceOf
      case tree: N.MatchCase => transform(tree).asInstanceOf
      case tree: N.Error => transform(tree).asInstanceOf
      case tree: N.WildcardPattern => transform(tree).asInstanceOf
      case tree: N.IdPattern => transform(tree).asInstanceOf
      case tree: N.LiteralPattern[_] => transform(tree).asInstanceOf
      case tree: N.CaseClassPattern => transform(tree).asInstanceOf
      case tree: N.ClassTypeTree => transform(tree).asInstanceOf
      case tree: N.TTypeTree => transform(tree).asInstanceOf
      case tree: N.FunctionTypeTree => transform(tree).asInstanceOf
  end transform

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  final def transformChildren(tree: N.Tree)(using Context): tree.type =
    tree match
      case N.Program(modules) =>
        for module <- modules do transform(module)
      case N.ModuleDef(_, defs, expr) =>
        for defn <- defs do transform(defn)
        for expr <- expr do transform(expr)
      case N.FunDef(_, tparams, vparams, ret, body) =>
        for tparam <- tparams do transform(tparam)
        for vparam <- vparams do transform(vparam)
        transform(ret)
        transform(body)
      case N.AbstractClassDef(_) => ()
      case N.CaseClassDef(_, fields, _) =>
        for field <- fields do transform(field)
      case N.ValParamDef(_, tpe) => transform(tpe)
      case N.TypeParamDef(_) => ()
      case N.EmptyExpr() => ()
      case N.Variable(_) => ()
      case N.FunRef(_) => ()
      case N.IntLiteral(_) => ()
      case N.BooleanLiteral(_) => ()
      case N.StringLiteral(_) => ()
      case N.UnitLiteral() => ()
      case N.InfixCall(lhs, _, rhs) =>
        transform(lhs)
        transform(rhs)
      case N.Call(_, targs, vargs) =>
        for targ <- targs do transform(targ)
        for varg <- vargs do transform(varg)
      case N.Not(tree) => transform(tree)
      case N.Neg(tree) => transform(tree)
      case N.Sequence(e1, e2) =>
        transform(e1)
        transform(e2)
      case N.Let(df, rhs, body) =>
        transform(df)
        transform(rhs)
        transform(body)
      case N.Ite(cond, thenn, elze) =>
        transform(cond)
        transform(thenn)
        transform(elze)
      case N.Match(scrut, cases) =>
        transform(scrut)
        for `case` <- cases do transform(`case`)
      case N.MatchCase(pat, expr) =>
        transform(pat)
        transform(expr)
      case N.Error(tree) => transform(tree)
      case N.WildcardPattern() => ()
      case N.IdPattern(_) => ()
      case N.LiteralPattern(lit) => transform(lit)
      case N.CaseClassPattern(_, args) =>
        for arg <- args do transform(arg)
      case N.ClassTypeTree(_) => ()
      case N.TTypeTree(_) => ()
      case N.FunctionTypeTree(args, ret) =>
        for arg <- args do transform(arg)
        transform(ret)
    tree
  end transformChildren

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree: N.Program)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree: N.ModuleDef)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree : N.FunDef)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.AbstractClassDef)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.CaseClassDef)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.ValParamDef)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.TypeParamDef)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.EmptyExpr)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Variable)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.FunRef)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.IntLiteral)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.BooleanLiteral)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.StringLiteral)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.UnitLiteral)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.InfixCall)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Call)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Not)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Neg)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Sequence)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Let)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Ite)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Match)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.MatchCase)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Error)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.WildcardPattern)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.IdPattern)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.LiteralPattern[_])(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.CaseClassPattern)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.ClassTypeTree)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.TTypeTree)(using Context): tree.type =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.FunctionTypeTree)(using Context): tree.type =
    transformChildren(tree)
  end transform
