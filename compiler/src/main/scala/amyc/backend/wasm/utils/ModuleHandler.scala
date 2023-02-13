package amyc.backend.wasm.utils

import amyc.backend.wasm.*
import amyc.core.Context
import amyc.core.Symbols.*
import amyc.utils.UniqueCounter

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class ModuleHandler(val name: String) :
  
  /* Atomic counter for constructors and functions */
  private val constrIndexes = new AtomicInteger
  private val funIndexes = new AtomicInteger
  
  private val _freemem = new AtomicInteger
  
  private val _constr: mutable.HashMap[Symbol, Int] = mutable.HashMap.empty
  private val _fun: mutable.HashMap[Symbol, Int] = mutable.HashMap.empty

  /* Pool of String literals to optimize strings in memory */
  private val _strpool: mutable.HashMap[String, Int] = mutable.HashMap.empty
  
  /* Compute the index of a constructor (id of a case class) */
  def constructor(sym: Symbol): Int =
    _constr.getOrElseUpdate(sym, constrIndexes.getAndIncrement)
  
  /* Compute the index of a function in the table */
  def function(sym: Symbol): Int =
    _fun.getOrElseUpdate(sym, funIndexes.getAndIncrement)
    
  def table: Table =
    val syms = _fun.toList.sorted((lhs, rhs) => lhs._2 - rhs._2).map(_._1.asInstanceOf[FunctionSymbol])
    Table(syms)
    
  // ==============================================================================================
  // ===================================== MEMORY MANAGEMENT ======================================
  // ==============================================================================================
  
  /* allocate memory */
  def alloc(size: Int): Int = 
    assert(size >= 0)
    _freemem.getAndAdd(size)
  
  /* allocating memory of size 0 will return the offset to the next free memory */
  def boundary : Int = alloc(0)
  
  // ==============================================================================================
  // ===================================== STRING MANAGEMENT ======================================
  // ==============================================================================================

  /* Find the reference in memory to the string or create a new one */
  def string(str: String) : Int =
    _strpool.getOrElseUpdate(str, alloc(str.length + 1))

  def strpool: List[Data] =
    _strpool.toList.map(s => Data(s._2, s._1))