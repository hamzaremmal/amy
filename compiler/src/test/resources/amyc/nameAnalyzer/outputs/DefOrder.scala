module DefOrder_0
  fn bar_0(): Foo_0 = {
    foo_0()
  }
  fn foo_0(): Foo_0 = {
    Bar_0(42)
  }
  case class Bar_0(v: Int_0) : Foo_0
  abstract class Foo_0
end DefOrder_0

