#include <wchar.h>
#include "testharness.h"

void printchars(wchar_t* str)
{
  while (1){
    printf("0x%x", *str);
    if (*str == 0) {
      printf("\n\n");
      return;
    }
    printf(", ");
    str++;
  }  
}

int main(){
  wchar_t wa[] = L"H" L"i\xabcd" "e"; 
  // wa == L"Hi\xabcd\x65". Since 'e' was in a separate token from \xabcd,
  //it is not part of the escape.  Instead, it's a regular old 'e' (ascii 65h).
  char a[] = "H\0i\0\xcd\xab\x65";

   wchar_t wb[] = L"Hi\300";
   unsigned char b[] = "Hi\300";

   int i;
   char* tmp = (char*)wa;
   for (i = 0; i < 6; i++) { //byte-for-byte compare
     if (tmp[i] != a[i]) E(1);
   }
   for (i = 0; i < 4; i++) { //char-to-wchar_t compare
     if (b[i] != (unsigned char)wb[i]) E(2);
   }
   
   printchars(wa);
   printchars(wb);

   return 0;
}
