package amyc.backend.wasm

import amyc.*
import amyc.core.Context
import amyc.utils.*
import amyc.backend.wasm.Values.*
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.types.{local as tlocal, *}
import amyc.backend.wasm.utils.*

// TODO HR : Remove this object and mix it with the WATFileGenerator

// Printer for Wasm modules
object ModulePrinter {
  private implicit def s2d(s: String): Raw = Raw(s)

  private def mkMod(mod: Module)(using Context): Document = Stacked(
    s"(module ${str2id(mod.name)}",
    Indented(Stacked(mod.imports map mkImport)),
    Indented(Stacked(mod.data map mkData)),
    Indented(Stacked(mod.globals.map(mkGlobal))),
    Indented(mkTable(mod.table.get)),
    Indented(Stacked(defaultFunTypes.map(s2d))),
    Indented(Stacked(mod.functions map mkFun)),
    ")"
  )

  def mkTable(table: Table): Document =
    val elem: List[Document] = (for f <- table.elems yield Indented(s"${str2id(fullName(f.owner, f))} ")) ::: Raw(")") :: Nil
    val header = Stacked(
      s"(table ${table.elems.size} funcref)",
      "(elem (i32.const 0)")
    Stacked{
      header :: elem
    }

  def mkGlobal(global: Global): Document =
    s"(global ${str2id("free_mem")} (mut i32) i32.const ${global.value})"

  def mkParam(p: param): Document =
    p.id.map(id => s"(param $id ${p.tpe})").getOrElse(s"(param ${p.tpe})")

  def mkResult(res: result): Document =
    s"(result ${res.tpe})"

  def mkLocal(p: tlocal): Document =
    p.id.map(id => s"(local $id ${p.tpe})").getOrElse(s"(local ${p.tpe})")

  def mkData(p: Data): Document =
    s"""(data (i32.const ${p.offset}) "${p.str}\\00")"""

  def mkTypeUse(tpe: typeuse): Document =
    s"(type ${tpe.x})"

  private def mkImport(s: String): Document =
    Lined(List("(import ", s, ")"))

  private def mkFun(fh: Function): Document = {
    val name = fh.name
    val exportDoc: Document = s"""(export "$name" (func $name))"""
    val paramsDoc: Document = Lined(fh.params.map(mkParam), " ")
    val resultDoc: Document = fh.result.map(mkResult).getOrElse("")
    val localsDoc: Document = Lined(fh.locals.map(mkLocal), " ")

    Stacked(
      exportDoc,
      Lined(List(s"(func ${fh.name} ", paramsDoc, resultDoc, localsDoc)),
      Indented(Stacked(mkCode(fh.code))),
      ")"
    )
  }

  private def mkCode(code: Code): List[Document] = code.instructions match {
    case Nil => Nil
    case h :: t => h match {
      case `else`(_) =>
        Unindented(mkInstr(h)) ::
        mkCode(t)
      case _ : end.type =>
        Unindented(mkInstr(h)) ::
        (mkCode(t) map Unindented.apply)
      case `if`(_, _) | Block(_) | Loop(_, _) =>
        mkInstr(h) ::
        (mkCode(t) map Indented.apply)
      case _ =>
        mkInstr(h) ::
        mkCode(t)
    }
  }

  private def mkInstr(instr: Instruction): Document = {
    instr match {
      case i32.const(value) => s"i32.const $value"
      case i32.add => "i32.add"
      case i32.sub => "i32.sub"
      case i32.mul => "i32.mul"
      case i32.div_s => "i32.div_s"
      case i32.rem_s => "i32.rem_s"
      case i32.rem_u => "i32.rem_u"
      case i32.and => "i32.and"
      case i32.or  => "i32.or"
      case i32.xor => "i32.xor"
      case i32.eqz => "i32.eqz"
      case i32.lt_s => "i32.lt_s"
      case i32.le_s => "i32.le_s"
      case _ : i32.eq.type => "i32.eq"
      case _ : drop.type => "drop"
      case `if`(label, tpe) => Lined(List(s"if", tpe.map(mkResult).getOrElse("")), " ")
      case `else`(id) => s"else ${id.getOrElse("")}"
      case Block(label) => s"block $$$label"
      case Loop(label, tpe) =>
        tpe match
          case None => s"loop $$$label"
          case Some(t) => Lined(s"loop $$$label" :: mkResult(t) :: Nil, " ")
      case br(label)=> s"br $label"
      case _ : `return`.type => "ret"
      case _ : end.type => "end"
      case call(name) => s"call $name"
      case call_indirect(tpe, idx) => Lined(List(s"call_indirect", s"$idx", mkTypeUse(tpe)), " ")
      case _ : unreachable.type => "unreachable"
      case local.get(index) => s"local.get $index"
      case local.set(index) => s"local.set $index"
      case global.get(index) => s"global.get $index"
      case global.set(index) => s"global.set $index"
      case i32.store => "i32.store"
      case i32.load => "i32.load"
      case i32.store8 => "i32.store8"
      case i32.load8_u => "i32.load8_u"
      case _ : i32.ne.type => "i32.ne"
      case Comment(s) =>
        var first = true;
        Stacked(s.split('\n').toList.map(s =>
          if (first) {
            first = false;
            Raw(s";;> $s")
          } else {
            Raw(s";;| $s")
          }
        ))
      case _ => throw new Exception(instr.toString)
    }
  }

  def apply(mod: Module)(using Context) = mkMod(mod).print
  def apply(fh: Function)(using Context) = mkFun(fh).print
  def apply(instr: Instruction)(using Context) = mkInstr(instr).print

}
