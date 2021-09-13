#include "testharness.h"
#define type1(x) _Generic((x), char: 1, unsigned int:2, default:0)
#define type2(x) _Generic((x), char: 1, unsigned int:2, const int:3, default:0)
#define type3(x) _Generic((x), int:1, const int:2, default:0)

int main() {
    unsigned char v_uchar;
    char v_char;
    int v_int;
    const int v_intconst;

    if(type1(v_int) != 0) { E(1); }
    if(type1(v_uchar) != 0) { E(2); }
    if(type1(v_char) != 1) { E(3); }

    if(type2(v_int) != 0) { E(4); }
    if(type2(v_intconst) != 0) { E(5); }
    
    if(type3(v_int) != 1) { E(6); }
    if(type3(v_intconst) != 1) { E(7); }

    if(type3((const int)v_int) != 1) { E(8); }
    if(type3((const int)v_intconst) != 1) { E(9); }

    SUCCESS;
}
