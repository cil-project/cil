//Make sure everything runs as is
//KEEP baseline: success

#include "../small1/testharness.h"


typedef struct parent {
  void * __RTTI * vtbl;
  int  *fseq;
  int  *f1;
} Parent;

#pragma ccured_extends("Schild", "Sparent")

typedef struct child {
  void * __RTTI * vtbl;
  int  * __FSEQ fseq;
  int  *f1;
  int  f2;
} Child;


//OpenSSL does casts between union fields like this.
union {
  int i;
  void* vp;
  int* ip;
  double d;
  Parent * __RTTI pp;
  Child *  cp;
} __TAGGED u;

int global[11];

int* foo(int* x) { return x; }

int main() {
  Child carray[5];
  Parent parray[2];

  u.ip = foo(&global[0]);
  unsigned long x = u.vp;
  x += u.i;
  u.ip++;  //KEEP fseq: success
  x += *u.ip;

  u.i = x; //KEEP wrongfield: error = wrong union field
  void* v = u.vp;

  u.cp = &carray[2];
  Parent * __RTTI p= u.pp;
  if (__endof(p) != (unsigned long)(carray + 5)) E(2);  //KEEP fseq
  Child * c = p;
  c++;           //KEEP fseq
  x += c->f2;

  u.vp = p; //make sure we preserve the RTTI.
  x += u.cp->f2;

  u.d = 1.0 / 10;  //DROP double: error = wrong union field
  double dd = u.d;

  SUCCESS;
}
