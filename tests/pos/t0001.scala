object t0001

  fn testMatch(l : L.List) : Unit = {
    l match {
      case L.Cons(a, b) =>
        Std.printInt(a);
        testMatch(b)
      case L.Nil() => ()
    }
  }

val l : L.List = L.Cons(1, L.Nil());
testMatch(l)

end t0001