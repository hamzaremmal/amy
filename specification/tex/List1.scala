object L 
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int(32), t: List) extends List
end L
