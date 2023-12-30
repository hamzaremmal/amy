package amyc
package transform

import ast.NominalTreeModule as N
import core.Context

/**
 * @author Hamza REMMAL (hamza@remmal.net)
 */
trait UntypedTreeTransformer:

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
      case N.Program(modules) => N.Program(modules.map(transform)).asInstanceOf
      case N.ModuleDef(name, defs, expr) =>
        N.ModuleDef(name, for defn <- defs yield transform(defn), for expr <- expr yield transform(expr)).asInstanceOf
      case N.FunDef(name, tparams, vparams, ret, body) =>
        N.FunDef(
          name,
          for tparam <- tparams yield transform(tparam),
          for vparam <- vparams yield transform(vparam),
          transform(ret),
          transform(body)
        ).asInstanceOf
      case N.AbstractClassDef(name) =>
        N.AbstractClassDef(name).asInstanceOf
      case N.CaseClassDef(name, fields, parent) =>
        N.CaseClassDef(name, for field <- fields yield transform(field), parent).asInstanceOf
      case N.ValParamDef(name, tpe) => N.ValParamDef(name, transform(tpe)).asInstanceOf
      case N.TypeParamDef(name) => N.TypeParamDef(name).asInstanceOf
      case N.EmptyExpr() => N.EmptyExpr().asInstanceOf
      case N.Variable(name) => N.Variable(name).asInstanceOf
      case N.FunRef(name) => N.FunRef(name).asInstanceOf
      case N.IntLiteral(value) => N.IntLiteral(value).asInstanceOf
      case N.BooleanLiteral(value) => N.BooleanLiteral(value).asInstanceOf
      case N.StringLiteral(value) => N.StringLiteral(value).asInstanceOf
      case N.UnitLiteral() => N.UnitLiteral().asInstanceOf
      case N.InfixCall(lhs, op, rhs) => N.InfixCall(transform(lhs), op, transform(rhs)).asInstanceOf
      case N.Call(fn, targs, vargs) =>
        N.Call(fn, for targ <- targs yield transform(targ), for varg <- vargs yield transform(varg)).asInstanceOf
      case N.Not(tree) => N.Not(transform(tree)).asInstanceOf
      case N.Neg(tree) => N.Neg(transform(tree)).asInstanceOf
      case N.Sequence(e1, e2) =>
        N.Sequence(transform(e1), transform(e2)).asInstanceOf
      case N.Let(df, rhs, body) =>
        N.Let(transform(df), transform(rhs), transform(body)).asInstanceOf
      case N.Ite(cond, thenn, elze) =>
        N.Ite(transform(cond), transform(thenn), transform(elze)).asInstanceOf
      case N.Match(scrut, cases) =>
        N.Match(transform(scrut), for `case` <- cases yield transform(`case`)).asInstanceOf
      case N.MatchCase(pat, expr) =>
        N.MatchCase(transform(pat), transform(expr)).asInstanceOf
      case N.Error(tree) => N.Error(transform(tree)).asInstanceOf
      case N.WildcardPattern() => N.WildcardPattern().asInstanceOf
      case N.IdPattern(name) => N.IdPattern(name).asInstanceOf
      case N.LiteralPattern(lit) => N.LiteralPattern(transform(lit)).asInstanceOf
      case N.CaseClassPattern(name, args) =>
        N.CaseClassPattern(name, for arg <- args yield transform(arg)).asInstanceOf
      case N.ClassTypeTree(name) => N.ClassTypeTree(name).asInstanceOf
      case N.TTypeTree(tpe) => N.TTypeTree(tpe).asInstanceOf
      case N.FunctionTypeTree(args, ret) =>
        N.FunctionTypeTree(for arg <- args yield transform(arg), transform(ret)).asInstanceOf
  end transformChildren

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree: N.Program)(using Context): N.Program =
    transformChildren(tree)
  end transform

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree: N.ModuleDef)(using Context): N.ModuleDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   * @param tree ???
   * @param x$2 ???
   */
  def transform(tree : N.FunDef)(using Context): N.FunDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.AbstractClassDef)(using Context): N.AbstractClassDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.CaseClassDef)(using Context): N.CaseClassDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.ValParamDef)(using Context): N.ValParamDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.TypeParamDef)(using Context): N.TypeParamDef =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.EmptyExpr)(using Context): N.Expr =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.Variable)(using Context): N.Expr =
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
  def transform(tree: N.WildcardPattern)(using Context): N.Pattern =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.IdPattern)(using Context): N.Pattern =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.LiteralPattern[_])(using Context): N.Pattern =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.CaseClassPattern)(using Context): N.Pattern =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.ClassTypeTree)(using Context): N.TypeTree =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.TTypeTree)(using Context): N.TypeTree =
    transformChildren(tree)
  end transform

  /**
   * ???
   *
   * @param tree ???
   * @param x$2  ???
   */
  def transform(tree: N.FunctionTypeTree)(using Context): N.TypeTree =
    transformChildren(tree)
  end transform
