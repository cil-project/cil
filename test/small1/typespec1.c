#include "testharness.h"

typedef unsigned char __u8;


__u8 short slot_tablelen;  // GCC seems to like this. And treat it as short
short slot2;


int main() {
  slot_tablelen = slot2 = 255; // This should fit
  if(slot_tablelen != slot2) E(1);

  slot_tablelen = slot2 = 65000; // This should fit
  if(slot_tablelen != slot2) E(2);

  slot_tablelen = slot2 = 130000; // This should fit
  if(slot_tablelen != slot2) E(3);


  SUCCESS;
  
}
