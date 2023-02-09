package amyc.backend.wasm.utils

import amyc.backend.wasm.Instructions.id
import amyc.backend.wasm.indices.localidx
import amyc.core.Symbols.*
import amyc.core.Context
import amyc.reporter

import scala.collection.mutable
import java.util.concurrent.atomic.AtomicInteger

final class LocalsHandler (val params : Int, textmode : Boolean = false) :

  private val locals_ = mutable.HashMap.empty[Symbol, localidx].withDefaultValue(-1)
  private val locals_counter : AtomicInteger = AtomicInteger(params)

  // Scala restriction : cannot have more two overloaded constructors with default parameters
  def this(args : List[Symbol], textmode : Boolean) =
    this(args.size, textmode)
    // reset the counter to start create locals from 0
    locals_counter.set(0)
    // Register all the arguments in the
    for arg <- args do getFreshLocal(arg)

  def apply(id: Symbol)(using Context): localidx =
  //locals_.getOrElse(id, {
  //  reporter.fatal(s"Cannot find $id in the LocalsHandler")
  //})
    locals_(id)

  def getFreshLocal(i: Symbol): localidx =
    locals_.getOrElseUpdate(i, {
      val v = getFreshLocal // increment the global counter
      if textmode then id(i.name) else v
    })


  def getFreshLocal : localidx =
    locals_counter.getAndIncrement()

  /* Number of defined locals in the scope of the handler */
  def locals: Int = locals_counter.get() - params
