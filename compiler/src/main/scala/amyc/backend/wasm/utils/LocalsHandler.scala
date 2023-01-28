package amyc.backend.wasm.utils

import amyc.ast.Identifier
import amyc.core.Context
import amyc.reporter

import scala.collection.mutable
import java.util.concurrent.atomic.AtomicInteger

// TODO HR : Add support for id by changing Int to localidx
// TODO HR : Add support for concurrency here
final class LocalsHandler (val params : Int) :

  private val locals_ = mutable.HashMap.empty[Identifier, Int].withDefaultValue(-1)
  private val locals_counter : AtomicInteger = AtomicInteger(params)

  def this(args : List[Identifier]) =
    this(args.size)
    // reset the counter to start create locals from 0
    locals_counter.set(0)
    // Register all the arguments in the
    for arg <- args do getFreshLocal(arg)

  def apply(id: Identifier)(using Context) :Int =
    //locals_.getOrElse(id, {
    //  reporter.fatal(s"Cannot find $id in the LocalsHandler")
    //})
    locals_(id)
  def getFreshLocal(id: Identifier): Int =
    locals_.getOrElseUpdate(id, getFreshLocal)

  def getFreshLocal : Int =
    locals_counter.getAndIncrement()

  /* Number of defined locals in the scope of the handler */
  def locals: Int = locals_counter.get() - params
