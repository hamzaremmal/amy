object NestedMatch
  foo match {
    case bar =>
      baz match {
        case 42 =>
          ()
      }
  }
end NestedMatch
