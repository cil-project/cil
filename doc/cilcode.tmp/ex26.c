  int foo (int predicate) {
    if (predicate <= 0) {
      return 1;
    } else {
      if (predicate > 5)
        return 2;
      return 3;
    }
  }
