package amyc.backend.wasm.utils

import amyc.core.Context
import amyc.core.Symbols.*
import amyc.utils.UniqueCounter

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class ModuleHandler :
  
  /* Atomic counter for constructors and functions */
  private val constrIndexes = new AtomicInteger
  private val funIndexes = new AtomicInteger
  
  private val _constr: mutable.HashMap[Symbol, Int] = mutable.HashMap.empty
  private val _fun: mutable.HashMap[Symbol, Int] = mutable.HashMap.empty
  
  /* Compute the index of a constructor (id of a case class) */
  def constructor(sym: Symbol): Int =
    _constr.getOrElseUpdate(sym, constrIndexes.getAndIncrement)
  
  /* Compute the index of a function in the table */
  def function(sym: Symbol): Int =
    _fun.getOrElseUpdate(sym, funIndexes.getAndIncrement)