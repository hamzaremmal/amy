fn range(from: Int(32), to: Int(32)): List = {
  if (to < from) { Nil() }
  else {
    Cons(from, range(from + 1, to))
  }
}
