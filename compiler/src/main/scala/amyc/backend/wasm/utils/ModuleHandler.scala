package amyc.backend.wasm.utils

import amyc.backend.wasm.Table
import amyc.core.Context
import amyc.core.Symbols.*
import amyc.utils.UniqueCounter

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class ModuleHandler(val name: String) :
  
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
    
  def table: Table =
    val syms = _fun.toList.sorted((lhs, rhs) => lhs._2 - rhs._2).map(_._1.asInstanceOf[FunctionSymbol])
    Table(syms)