module Locals_0
  fn foo_0(): Int_0 = {
    (if(true) {
      (
        val i_0: Int_0 =
          42;
        i_0
      )
    } else {
      42 match {
        case i_1 =>
          42
      }
    })
  }
end Locals_0
module String_0
  abstract class String_1
  fn concat_0(lhs_0: String_1, rhs_0: String_1): String_1 = {
    error("Stub implementation")
  }
end String_0
