fn length(l: List): Int(32) = { l match {
  case Nil() => 0
  case Cons(h, t) => 1 + length(t)
}}
