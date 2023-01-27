package amyc.backend.wasm

import amyc.*
import amyc.core.Context
import amyc.utils.*
import amyc.backend.wasm.instructions.Instructions.*
import amyc.backend.wasm.instructions.*
import amyc.backend.wasm.instructions.numeric.i32
import amyc.backend.wasm.types.result
import variable.*
import amyc.backend.wasm.utils.Utils

// TODO HR : Remove this object and mix it with the WATFileGenerator

// Printer for Wasm modules
object ModulePrinter {
  private implicit def s2d(s: String): Raw = Raw(s)

  private def mkMod(mod: Module)(using Context): Document = Stacked(
    "(module ",
    Indented(Stacked(mod.imports map mkImport)),
    Indented("(global (mut i32) i32.const 0) " * mod.globals),
    Indented(mkTable(mod.table.get)),
    Indented(Stacked(Utils.defaultFunTypes.map(Raw))),
    Indented(Stacked(mod.functions map mkFun)),
    ")"
  )

  def mkTable(table: Table): Document =
    val elem: List[Document] = (for f <- table.elems yield Indented(s"$$${f.name} ")) ::: Raw(")") :: Nil
    val header = Stacked(
      s"(table ${table.size} funcref)",
      "(elem (i32.const 0)")
    Stacked{
      header :: elem
    }

  def mkResult(res: result): Document =
    s"(result ${res.tpe})"


  private def registerFunction(fn: List[Function])(using Context): Document =
    val names = for f <- fn.sorted(_.idx - _.idx) yield s"$$${f.name}"
    reporter.info(s"${fn.map(_.idx)}")
    Raw(s"(elem (i32.const 0) ${names.mkString(" ")})")

  private def mkImport(s: String): Document =
    Lined(List("(import ", s, ")"))

  private def mkFun(fh: Function): Document = {
    val name = fh.name
    val isMain = fh.isMain
    val exportDoc: Document = if (isMain) s"""(export "$name" (func $$$name))""" else ""
    val paramsDoc: Document = if (fh.args == 0) "" else {
      Lined(List(
        "(param ",
        Lined(List.fill(fh.args)(Raw("i32")), " "),
        ") "
      ))
    }
    val resultDoc: Document = if (isMain) "" else "(result i32) "
    val localsDoc: Document =
      if (fh.locals > 0)
        "(local " <:> Lined(List.fill(fh.locals)(Raw("i32")), " ") <:> ")"
      else
        ""

    Stacked(
      exportDoc,
      Lined(List(s"(func $$${fh.name} ", paramsDoc, resultDoc, localsDoc)),
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
      case `if`(_, _) | Block(_) | Loop(_) =>
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
      case Loop(label) => s"loop $$$label"
      case br(label)=> s"br $label"
      case instructions.`return` => "ret"
      case _ : end.type => "end"
      case call(name) => s"call $name"
      case CallIndirect(tpe) => s"call_indirect (type $tpe)"
      case instructions.unreachable => "unreachable"
      case local.get(index) => s"local.get $index"
      case local.set(index) => s"local.set $index"
      case global.get(index) => s"global.get $index"
      case global.set(index) => s"global.set $index"
      case i32.store => "i32.store"
      case i32.load => "i32.load"
      case i32.store8 => "i32.store8"
      case i32.load8_u => "i32.load8_u"
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
