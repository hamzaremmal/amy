package amyc.backend.wasm.utils

import amyc.core.Context
import amyc.core.Symbols.*
import amyc.backend.wasm.*
import amyc.backend.wasm.Values.*
import amyc.backend.wasm.Instructions.*
import amyc.utils.UniqueCounter

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

case class ModuleHandler(name: String) :
  
  /* Constructor indexes management */
  private val constrIndexes = new AtomicInteger
  private val _constr: mutable.HashMap[Symbol, Int] = mutable.HashMap.empty

  /* Function indexes management */
  private val funIndexes = new AtomicInteger
  private val _fun: mutable.HashMap[Symbol, Int] = mutable.HashMap.empty

  /* Memory management related */
  private val _freemem = new AtomicInteger
  private lazy val _freemem_id = str2id("free_mem")

  /* Pool of String literals to optimize strings in memory */
  private val _strpool: mutable.HashMap[String, Int] = mutable.HashMap.empty

  // ==============================================================================================
  // ========================================= API ================================================
  // ==============================================================================================
  
  /* Compute the index of a constructor (id of a case class) */
  def constructor(sym: Symbol): Int =
    _constr.getOrElseUpdate(sym, constrIndexes.getAndIncrement)
  
  /* Compute the index of a function in the table */
  def function(sym: Symbol): Int =
    _fun.getOrElseUpdate(sym, funIndexes.getAndIncrement)

  /* Create the table with reference to all the registered functions */
  def table: Table =
    Table{
      for f <- _fun.toList.sorted((lhs, rhs) => lhs._2 - rhs._2)
      yield
        f._1.asInstanceOf[FunctionSymbol]
    }
    
  // ==============================================================================================
  // ===================================== MEMORY MANAGEMENT ======================================
  // ==============================================================================================

  /**
    * Allocate statically memory
    * This is used for String literals for now
    * @param size (Int) - size of the memory to allocate
    * @return (Int) - base address of the allocated memory
    */
  def static_alloc(size: Int): Int =
    assert(size >= 0)
    _freemem.getAndAdd(size)

  /**
    * Base address of the next free memory
    * @return (Int) - Base address of the next free memory
    */
  def static_boundary : Int =
    // allocating memory of size 0 will return the offset to the next free memory
    static_alloc(0)

  /**
    * Allocate dynamically memory
    *
    * It assumes the size is in the stack and leaves the base address there.
    * @return
    */
  def dynamic_alloc(using LocalsHandler) : Code =
    val size = lh.getFreshLocal
    // Store the size in a local variable (formula for adtField)
    i32.const(1) <:>
    i32.add <:>
    i32.const(4) <:>
    i32.mul <:>
    local.set(size) <:>
    Comment(s"dynamically allocating memory") <:>
    // Leave the base address in the stack
    global.get(_freemem_id) <:>
    // Computing the new offset of free memory
    global.get(_freemem_id) <:>
    local.get(size) <:>
    i32.add <:>
    // Storing the new size in memory
    global.set(_freemem_id) <:>
    Comment(s"end of dynamic allocation")

  /**
    * Allocate dynamically memory.
    *
    * It leaves in the stack the base address for the object
    * @param size (Int) - size to allocate
    * @return (Code) - code to execute to allocate memory dynamically
    *         leaves in the stack the base address (i32)
    */
  def dynamic_alloc(size: Int)(using LocalsHandler): Code =
    i32.const(size) <:>
    dynamic_alloc


  /**
    * Return dynamically the address of the next free memory
    * @return (Code) - code to execute to get the memory boundary
    *         leaves the address (i32) in the stack
    */
  def dynamic_boundary: Code =
    global.get(_freemem_id)

  /**
    * Dynamically change the memory boundary
    *
    * THIS FUNCTION SHOULD BE REMOVED
    * FOR NOW, IT'S SHOULD ONLY BE USED IN Std::readString
    * @param code
    * @return
    */
  def dynamic_boundary(code: Code): Code =
    code <:>
    global.set(_freemem_id)

  // ==============================================================================================
  // ===================================== STRING MANAGEMENT ======================================
  // ==============================================================================================

  /**
    * Fetch the address in memory of a String.
    *
    * If String is not in memory, it'll allocate memory
    * @param str (String) - String to fetch
    * @return (Int) - base address of the String
    */
  def string(str: String) : Int =
    _strpool.getOrElseUpdate(str, static_alloc(str.length + 1))

  /**
    * Build the Data segments with the stored Strings in memory
    * @return (List[Data]) - List of data segments
    */
  def strpool: List[Data] =
    for (str, address) <- _strpool.toList yield
      Data(address, str)