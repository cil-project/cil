#include <wchar.h>
#include "testharness.h"

/*
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
*/

int main(){
  wchar_t wa[] = L"H" L"i\xabcd" "e"; 
  // wa == L"Hi\xabcd\x65". Since 'e' was in a separate token from \xabcd,
  //it is not part of the escape.  Instead, it's a regular old 'e' (ascii 65h).
 
  //wa should equal one of these byte strings:
  char *a_16bit = "H\0i\0\xcd\xab\x65\0";
  char *a_32bit = "H\0\0\0i\0\0\0\xcd\xab\0\0\x65\0\0\0";

   wchar_t wb[] = L"Hi\300";
   unsigned char b[] = "Hi\300";

   int i;
   if (sizeof(wchar_t) == 16){
     char* tmp = (char*)wa;
     for (i = 0; i < 4*2; i++) { //byte-for-byte compare
       if (tmp[i] != a_16bit[i]) E(1);
     }
   }
   else
   {
     char* tmp = (char*)wa;
     for (i = 0; i < 4*4; i++) { //byte-for-byte compare
       if (tmp[i] != a_32bit[i]) E(1);
     }
   }
   for (i = 0; i < 4; i++) { //char-to-wchar_t compare
     if (b[i] != (unsigned char)wb[i]) E(2);
   }
   
   //printchars(wa);
   //printchars(wb);

   return 0;
}
