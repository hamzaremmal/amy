module ConstructorVsId_0
  abstract class Foo_0
  case class Bar_0() : Foo_0
  fn foo_0(f_0: Foo_0): Int_0 = {
    f_0 match {
      case Bar_0() =>
        42
      case Bar_1 =>
        42
    }
  }
end ConstructorVsId_0
module String_0
  abstract class String_1
  fn concat_0(lhs_0: String_1, rhs_0: String_1): String_1 = {
    error("Stub implementation")
  }
end String_0
