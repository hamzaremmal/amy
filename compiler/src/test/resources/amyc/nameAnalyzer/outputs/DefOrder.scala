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
module Unit_0
  abstract class Unit_1
end Unit_0
module Int_1
  abstract class Int_0
end Int_1
module Boolean_0
  abstract class Boolean_1
end Boolean_0
module unnamed_0
  fn +_0(lhs_0: Int_0, rhs_0: Int_0): Int_0 = {
    <empty>
  }
  fn -_0(lhs_1: Int_0, rhs_1: Int_0): Int_0 = {
    <empty>
  }
  fn *_0(lhs_2: Int_0, rhs_2: Int_0): Int_0 = {
    <empty>
  }
  fn /_0(lhs_3: Int_0, rhs_3: Int_0): Int_0 = {
    <empty>
  }
  fn %_0(lhs_4: Int_0, rhs_4: Int_0): Int_0 = {
    <empty>
  }
  fn <_0(lhs_5: Int_0, rhs_5: Int_0): Boolean_1 = {
    <empty>
  }
  fn <=_0(lhs_6: Int_0, rhs_6: Int_0): Boolean_1 = {
    <empty>
  }
  fn &&_0(lhs_7: Boolean_1, rhs_7: Boolean_1): Boolean_1 = {
    <empty>
  }
  fn ||_0(lhs_8: Boolean_1, rhs_8: Boolean_1): Boolean_1 = {
    <empty>
  }
  fn ++_0(lhs_9: String_0, rhs_9: String_0): String_0 = {
    <empty>
  }
end unnamed_0
module String_1
  abstract class String_0
  fn concat_0(lhs_10: String_0, rhs_10: String_0): String_0 = {
    <empty>
  }
  fn length_0(str_0: String_0): Int_0 = {
    <empty>
  }
end String_1