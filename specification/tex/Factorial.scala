object Factorial 
  fn fact(i: Int(32)): Int(32) = {
    if (i < 2) { 1 }
    else { i * fact(i-1) }
  }
end Factorial
