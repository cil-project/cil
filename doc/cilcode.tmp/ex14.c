  int x = 5; 
  int main() {
    int x = 6;
    { 
      static int x = 7;
      return x;
    }
    return x;
  } 
