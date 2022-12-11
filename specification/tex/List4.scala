fn head(l: List): Int(32) = {
  l match {
    case Cons(h, _) => h
    case Nil() => error("head(Nil)")
  }
}
