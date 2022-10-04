object Hanoi
	
  fn solve(n : Int(32)) : Int(32) = {
    if (n < 1) { 
      error("can't solve Hanoi for less than 1 plate")
    } else {
      if (n == 1) {
        1
      } else {
        2 * solve(n - 1) + 1
      }
    }
  }

  Std.printString("Hanoi for 4 plates: " ++ Std.intToString(solve(4)))
end Hanoi