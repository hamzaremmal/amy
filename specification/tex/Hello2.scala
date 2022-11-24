object ReadName 
  Std.printString("What is your name?");
  val name: String = Std.readString();
  Std.printString("Hello " ++ name)
end ReadName
