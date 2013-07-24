  int dangerous() {
    char array[10];
    scanf("%s",array); // possible buffer overrun!
  }

  int main () {
    return dangerous();
  }
