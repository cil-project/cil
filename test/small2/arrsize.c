#include "../small1/testharness.h"

// NUMERRORS 7

int g1[-1]; // ERROR(1):Length of array is negative

#define MAXINT (1ull << ((8 * sizeof(int)) - 1))

int g1[ MAXINT / sizeof(int)  ]; //ERROR(2):Length of array is too large
int g1[ MAXINT / sizeof(int) - 1 ];//ERROR(3):Error 3

char g1[ MAXINT / sizeof(char) ]; //ERROR(4):Length of array is too large
char g1[ MAXINT / sizeof(char) - 1  ]; //ERROR(5):Error 5

double g1[ MAXINT / sizeof(double) ]; //ERROR(6):Length of array is too large
double g1[ MAXINT / sizeof(double) - 1  ]; //ERROR(7):Error 7

int main() {
  E(3); //ERROR(3)
  E(5); //ERROR(5)
  E(7); //ERROR(7)
  return 0;
}
