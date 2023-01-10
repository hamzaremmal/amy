object Classes_0
  abstract class Foo_0
  case class Bar_0() extends Foo_0
  case class Baz_0(v: Int(32), v: Foo_0) extends Foo_0
  fn foo_0(f_0: Foo_0): Foo_0 = {
    f_0 match {
      case Bar_0() =>
        Bar_0()
      case Baz_0(_, Bar_0()) =>
        Baz_0(0, Baz_0(0, Bar_0()))
      case Baz_0(1, Baz_0(j_0, ff_0)) =>
        Baz_0(j_0, Baz_0(1, Bar_0()))
    }
  }
end Classes_0

