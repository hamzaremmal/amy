package amyc
package transform

import ast.SymbolicTreeModule as S
import core.Context



/**
 * @author Hamza REMMAL (hamza@remmal.net)
 */
trait TypedTreeTransformer:

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  final def transform(tree: S.Tree)(using Context): tree.type =
    tree match
      case tree: S.Program => transform(tree).asInstanceOf
      case tree: S.ModuleDef => transform(tree).asInstanceOf
      case tree: S.FunDef => transform(tree).asInstanceOf
      case tree: S.AbstractClassDef => transform(tree).asInstanceOf
      case tree: S.CaseClassDef => transform(tree).asInstanceOf
      case tree: S.ValParamDef => transform(tree).asInstanceOf
      case tree: S.TypeParamDef => transform(tree).asInstanceOf
      case tree: S.EmptyExpr => transform(tree).asInstanceOf
      case tree: S.Variable => transform(tree).asInstanceOf
      case tree: S.FunRef => transform(tree).asInstanceOf
      case tree: S.IntLiteral => transform(tree).asInstanceOf
      case tree: S.BooleanLiteral => transform(tree).asInstanceOf
      case tree: S.StringLiteral => transform(tree).asInstanceOf
      case tree: S.UnitLiteral => transform(tree).asInstanceOf
      case tree: S.InfixCall => transform(tree).asInstanceOf
      case tree: S.Call => transform(tree).asInstanceOf
      case tree: S.Not => transform(tree).asInstanceOf
      case tree: S.Neg => transform(tree).asInstanceOf
      case tree: S.Sequence => transform(tree).asInstanceOf
      case tree: S.Let => transform(tree).asInstanceOf
      case tree: S.Ite => transform(tree).asInstanceOf
      case tree: S.Match => transform(tree).asInstanceOf
      case tree: S.MatchCase => transform(tree).asInstanceOf
      case tree: S.Error => transform(tree).asInstanceOf
      case tree: S.WildcardPattern => transform(tree).asInstanceOf
      case tree: S.IdPattern => transform(tree).asInstanceOf
      case tree: S.LiteralPattern[_] => transform(tree).asInstanceOf
      case tree: S.CaseClassPattern => transform(tree).asInstanceOf
      case tree: S.ClassTypeTree => transform(tree).asInstanceOf
      case tree: S.TTypeTree => transform(tree).asInstanceOf
      case tree: S.FunctionTypeTree => transform(tree).asInstanceOf
  end transform

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  final def transformChildren(tree: S.Tree)(using Context): tree.type =
    tree match
      case S.Program(modules) => S.Program(modules.map(transform)).asInstanceOf
      case S.ModuleDef(name, defs, expr) =>
        S.ModuleDef(name, for defn <- defs yield transform(defn), for expr <- expr yield transform(expr)).asInstanceOf
      case S.FunDef(name, tparams, vparams, ret, body) =>
        S.FunDef(
          name,
          for tparam <- tparams yield transform(tparam),
          for vparam <- vparams yield transform(vparam),
          transform(ret),
          transform(body)
        ).asInstanceOf
      case S.AbstractClassDef(name) =>
        S.AbstractClassDef(name).asInstanceOf
      case S.CaseClassDef(name, fields, parent) =>
        S.CaseClassDef(name, for field <- fields yield transform(field), parent).asInstanceOf
      case S.ValParamDef(name, tpe) => S.ValParamDef(name, transform(tpe)).asInstanceOf
      case S.TypeParamDef(name) => S.TypeParamDef(name).asInstanceOf
      case S.EmptyExpr() => S.EmptyExpr().asInstanceOf
      case S.Variable(name) => S.Variable(name).asInstanceOf
      case S.FunRef(name) => S.FunRef(name).asInstanceOf
      case S.IntLiteral(value) => S.IntLiteral(value).asInstanceOf
      case S.BooleanLiteral(value) => S.BooleanLiteral(value).asInstanceOf
      case S.StringLiteral(value) => S.StringLiteral(value).asInstanceOf
      case S.UnitLiteral() => S.UnitLiteral().asInstanceOf
      case S.InfixCall(lhs, op, rhs) => S.InfixCall(transform(lhs), op, transform(rhs)).asInstanceOf
      case S.Call(fn, targs, vargs) =>
        S.Call(fn, for targ <- targs yield transform(targ), for varg <- vargs yield transform(varg)).asInstanceOf
      case S.Not(tree) => S.Not(transform(tree)).asInstanceOf
      case S.Neg(tree) => S.Neg(transform(tree)).asInstanceOf
      case S.Sequence(e1, e2) =>
        S.Sequence(transform(e1), transform(e2)).asInstanceOf
      case S.Let(df, rhs, body) =>
        S.Let(transform(df), transform(rhs), transform(body)).asInstanceOf
      case S.Ite(cond, thenn, elze) =>
        S.Ite(transform(cond), transform(thenn), transform(elze)).asInstanceOf
      case S.Match(scrut, cases) =>
        S.Match(transform(scrut), for `case` <- cases yield transform(`case`)).asInstanceOf
      case S.MatchCase(pat, expr) =>
        S.MatchCase(transform(pat), transform(expr)).asInstanceOf
      case S.Error(tree) => S.Error(transform(tree)).asInstanceOf
      case S.WildcardPattern() => S.WildcardPattern().asInstanceOf
      case S.IdPattern(name) => S.IdPattern(name).asInstanceOf
      case S.LiteralPattern(lit) => S.LiteralPattern(transform(lit)).asInstanceOf
      case S.CaseClassPattern(name, args) =>
        S.CaseClassPattern(name, for arg <- args yield transform(arg)).asInstanceOf
      case S.ClassTypeTree(name) => S.ClassTypeTree(name).asInstanceOf
      case S.TTypeTree(tpe) => S.TTypeTree(tpe).asInstanceOf
      case S.FunctionTypeTree(args, ret) =>
        S.FunctionTypeTree(for arg <- args yield transform(arg), transform(ret)).asInstanceOf
  end transformChildren

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree: S.Program)(using Context): S.Program =
    transformChildren(tree)
  end transform

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree: S.ModuleDef)(using Context): S.ModuleDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree : S.FunDef)(using Context): S.FunDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.AbstractClassDef)(using Context): S.AbstractClassDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.CaseClassDef)(using Context): S.CaseClassDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.ValParamDef)(using Context): S.ValParamDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.TypeParamDef)(using Context): S.TypeParamDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.EmptyExpr)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Variable)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.FunRef)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.IntLiteral)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.BooleanLiteral)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.StringLiteral)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.UnitLiteral)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.InfixCall)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Call)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Not)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Neg)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Sequence)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Let)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Ite)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Match)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.MatchCase)(using Context): S.MatchCase =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.Error)(using Context): S.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.WildcardPattern)(using Context): S.Pattern =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.IdPattern)(using Context): S.Pattern =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.LiteralPattern[_])(using Context): S.Pattern =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.CaseClassPattern)(using Context): S.Pattern =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.ClassTypeTree)(using Context): S.TypeTree =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.TTypeTree)(using Context): S.TypeTree =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: S.FunctionTypeTree)(using Context): S.TypeTree =
    transformChildren(tree)
  end transform
