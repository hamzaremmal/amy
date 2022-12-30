object FunCalls
  fn foo(i: Int(32)): Int(32) = {
    i
  }
  fn bar(i: Int(32), j: Int(32)): Int(32) = {
    (i + j)
  }
  foo(1);
  val foz: Int(32) =
    4;
  foo(foz);
  foo((
    val f: Int(32) =
      42;
    f
  ));
  bar(1, 2);
  val baz: Int(32) =
    4;
  foo(foz, baz);
  foo((
    val f: Int(32) =
      42;
    f
  ), (
    val b: Int(32) =
      1;
    b
  ))
end FunCalls