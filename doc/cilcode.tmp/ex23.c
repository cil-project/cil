  int foo (int predicate) {
    int x = 0;
    switch (predicate) {
      case 0: return 111;
      case 1: x = x + 1;
      case 2: return (x+3);
      case 3: break;
      default: return 222;
    }
    return 333;
  }
