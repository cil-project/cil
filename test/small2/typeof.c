// simple use of typeof
  
int globalInt;

int main()
{
  __typeof(globalInt) localInt;
  localInt  = 6 * 2 - 12;
  return localInt;
}

