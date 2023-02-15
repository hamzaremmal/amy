package amyc.backend.wasm.utils

import amyc.backend.wasm.Instructions.{i32, id}
import amyc.backend.wasm.indices.localidx
import amyc.backend.wasm.types.{local, param}
import amyc.core.Symbols.*
import amyc.core.{Context, Identifier}
import amyc.{reporter, symbols}

import scala.collection.mutable
import java.util.concurrent.atomic.AtomicInteger

final class LocalsHandler(val sym: ApplicationSymbol, val mh: ModuleHandler, textmode: Boolean = true):

  // HR: Had to switch from HashMap to ListBuffer to keep the order
  private val params_ =
    mutable.ListBuffer.empty[(Symbol, localidx)]
  private val locals_ =
    mutable.ListBuffer.empty[(Symbol, localidx)]
  private val locals_counter: AtomicInteger = AtomicInteger(0)

  // Register all parameters
  for p <- sym.param do
    params_ += (p -> {
      val v = locals_counter.getAndIncrement // increment the global counter
      if textmode then id(p.fullName) else v
    })

  // ==============================================================================================
  // =========================================== API ==============================================
  // ==============================================================================================

  def fetch(id: Symbol)(using Context): localidx =
    params_.toMap.getOrElse(id, locals_.toMap.getOrElse(id, reporter.fatal("symbol was not found by LocalsHandler")))

  def getFreshLocal(i: Symbol): localidx =
    params_.toMap.getOrElse(
      i,
      locals_.toMap.getOrElse(
        i, {
          val entry: (Symbol, localidx) = i -> {
            val v =
              locals_counter.getAndIncrement // increment the global counter
            if textmode then id(i.fullName) else v
          }
          locals_ += entry
          entry._2
        }
      )
    )

  // Generate a synthetic symbol, guarantied to have different name
  def getFreshLocal: localidx =
    getFreshLocal(LocalSymbol(Identifier.fresh("x")))

  def params: List[param] =
    if textmode then
      params_.map(l => param(Some(l._2.asInstanceOf[id]), i32)).toList
    else
      params_.map(_ => param(None, i32)).toList

  def locals: List[local] =
    if textmode then
      locals_.map(l => local(Some(l._2.asInstanceOf[id]), i32)).toList
    else
      locals_.map(_ => local(None, i32)).toList

object LocalsHandler:
  implicit def lh2mh(lh: LocalsHandler): ModuleHandler = lh.mh