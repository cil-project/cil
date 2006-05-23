// simple use of typeof
  
int globalInt;

void y() { }

void typeofVoid() {
  (typeof(y()))0;
}


int main()
{
  __typeof(globalInt) localInt;
  localInt  = 6 * 2 - 12;
  return localInt;
}

