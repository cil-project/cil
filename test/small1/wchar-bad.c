#include <wchar.h>
#include "testharness.h"

// NUMERRORS 4


int check(char *p1, char *p2, int size, int code) {
  int i;
  for (i=0; i<size; i++ ){
    if (p1[i] != p2[i]) {
      E(code);
    }
  }
} 


int main(){
  unsigned char c1[] = "\7";
  unsigned char c2[] = "\77";

  //character too big:
  unsigned char c3[] = "\777";  //ERROR(1): too big

  //OK, because only the first three digits are part of the escape.
  unsigned char c4[] = "\1111"; 



  wchar_t w1[] = L"\xa";
  wchar_t w2[] = L"\xabcd";
  wchar_t w3[] = L"\xabcde";  //ERROR(2): too big
  wchar_t w4[] = L"\xcdefg"; //OK, g is not a hex digit.


  //type mismatches in array initialization:
  char s1[] = L"Hi"; //ERROR(3)
  wchar_t s2[] = "Hi"; //ERROR(4)

  SUCCESS;
}
