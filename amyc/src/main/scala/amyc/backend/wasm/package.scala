package amyc.backend

import amyc.backend.wasm.utils.LocalsHandler

package object wasm {

  // This function can be used to summon a LocalsHandler
  inline def lh(using LocalsHandler): LocalsHandler = summon

}
