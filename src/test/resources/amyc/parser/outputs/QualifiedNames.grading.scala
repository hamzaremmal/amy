object QualifiedNames
  val x: Faz.boo =
    Foo.bar(x match {
      case Foo.baz(foo) =>
        1
      case Foo.baz(Foo.bar(), foo) =>
        2
    });
  42
end QualifiedNames

