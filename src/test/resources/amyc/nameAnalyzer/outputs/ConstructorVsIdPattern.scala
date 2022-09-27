object ConstructorVsId_0
  abstract class Foo_0
  case class Bar_0() extends Foo_0
  fn foo_0(f_0: Foo_0): Int(32) = {
    f_0 match {
      case Bar_0() =>
        42
      case Bar_1 =>
        42
    }
  }
end ConstructorVsId_0

