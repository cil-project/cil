//Bug caused by overflow in bitsSizeOf (SourceForge #1641570)

//Partial workaround:  define "bytesSizeOf", not bitsSizeOf, to give us 8x more
//breathing room.
//Better workaround: make bitsSizeOf return an int64
//Current solution: a warning

char tab[1000000000];

//TODO: give somthing better than a warning here ...
extern char foo[sizeof(tab)];

char foo[1000000000];

int main () {
  int i;

  tab[999999999] = foo[5];
  i = sizeof (tab);

  return 0;
}
