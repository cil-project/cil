// Adapted from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/floats-cbmc-regression/float-rounding1.c
// For license see below
#include <assert.h>
void reach_error() { assert(0); }
#ifdef __GNUC__
#include <math.h>
#include <fenv.h>

// Should work without this as it defaults to off.
// It is explicitly ignored by GCC
#pragma STDC FENV_ACCESS OFF

void roundingTest (float f1, float f2) {
  // (Re)Set to the default rounding mode.
  fesetround(FE_TONEAREST);

 // With round to nearest, should get 0x1.000002p+0f
  float roundToNearestSum = f1 + f2;
  if(!(roundToNearestSum == 0x1.000002p+0f)) {reach_error();}

  // Change the rounding mode
  fesetround(FE_DOWNWARD);

  // Should now round down to 0x1p+0;
  float roundDownSum = f1 + f2;
  if(!(roundDownSum == 0x1.0p+0f)) {reach_error();}

  return;
}
#endif

int main (void)
{
  #ifdef __GNUC__
  float f1 = 0x1.0p+0;
  float f2 = 0x1.8p-24;

  // Test with constant folding
  roundingTest(f1,f2);

  #endif

  return 0;
}

/*
(C) 2001-2016, Daniel Kroening, Edmund Clarke,
Computer Science Department, University of Oxford
Computer Science Department, Carnegie Mellon University

All rights reserved. Redistribution and use in source and binary forms, with
or without modification, are permitted provided that the following
conditions are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

  3. All advertising materials mentioning features or use of this software
     must display the following acknowledgement:

     This product includes software developed by Daniel Kroening,
     Edmund Clarke,
     Computer Science Department, University of Oxford
     Computer Science Department, Carnegie Mellon University

  4. Neither the name of the University nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE. */
