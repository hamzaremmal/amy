object Arithmetic
  fn pow(b: Int(32), e: Int(32)): Int(32) = {
    if (e == 0) { 1 }
    else {
      if (e % 2 == 0) {
        val rec: Int(32) = pow(b, e/2);
        rec * rec
      } else {
        b * pow(b, e - 1)
      }
    }
  }

  fn gcd(a: Int(32), b: Int(32)): Int(32) = {
    if (a == 0 || b == 0) {
      a + b
    } else {
      if (a < b) {
        gcd(a, b % a)
      } else {
        gcd(a % b, b)
      }
    }
  }

  Std.printInt(pow(0, 10));
  Std.printInt(pow(1, 5));
  Std.printInt(pow(2, 10));
  Std.printInt(pow(3, 3));
  Std.printInt(gcd(0, 10));
  Std.printInt(gcd(17, 99)); // 1
  Std.printInt(gcd(16, 46)); // 2
  Std.printInt(gcd(222, 888)) // 222
end Arithmetic
