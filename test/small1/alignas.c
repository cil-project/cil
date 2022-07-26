#include <stdalign.h>

struct XXH3_state_s {
    _Alignas(int) unsigned char customSecret1[25];
    _Alignas(64) unsigned char customSecret2[25];
    alignas(64) unsigned char customSecret3[25];
};

int main() {

}
