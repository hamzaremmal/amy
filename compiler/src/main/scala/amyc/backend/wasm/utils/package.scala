package amyc.backend.wasm

import amyc.backend.wasm.utils.LocalsHandler

package object utils {

  // This function can be used to summon a LocalsHandler
  inline def lh(using LocalsHandler): LocalsHandler = summon

}
