object CaseClassDefs
  abstract class Foo
  case class Bar() : Foo
  case class Bar(v: Int) : Foo
  abstract class Foo
  case class Bar(v: Int, v: A) : Foo
end CaseClassDefs
