#include <assert.h>
#include <stdio.h>
int main(){
    // Previously, CIL assigned "-3" here.
    // This previously erroneous behavior cannot be really tested for, so it has to be manually inspected.
    unsigned long long x = 18446744073709551613ul;
    x = 18446744073709551613ul;
    if (x <= 18446744073709551612ull){
        assert(0);
    }
    return 0;
}
