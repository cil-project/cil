#include <stdlib.h>
#include <stdio.h>

#if 1
#define WILD
#define SAFE
#define TAGGED
#else
#define WILD __attribute__((wild))
#define SAFE __attribute__((safe))
#define TAGGED __attribute__((tagged))
#endif

typedef struct list {
  void * WILD car;
  struct list * SAFE cdr;
} LIST, * PLIST SAFE;


PLIST prepend(PLIST l, void * WILD el) {
  PLIST n = (PLIST)malloc(sizeof(LIST));
  n->car = el;
  n->cdr = l;
  return n;
}


PLIST append(PLIST l, void * WILD el) {
  PLIST parent = 0;
  PLIST n = l;
  while(n) {
    parent = n;
    n = n->cdr; 
  }
  n = (PLIST)malloc(sizeof(LIST));
  n->car = el;
  n->cdr = 0;
  if(parent) {
    parent->cdr = n;
    return l;
  } else {
    return n;
  }
}


PLIST insert(PLIST l, void * WILD el, int pos) {
  PLIST n = l;
  PLIST t;
  if(l) {
    while(n->cdr && pos > 0) {
      n = n->cdr;
    }
  }
  t = (PLIST)malloc(sizeof(LIST));
  if(! l) {
    t->cdr = NULL;
    return t;
  } else {
    t->cdr = n->cdr;
    n->cdr = t;
    return l;
  }
}

int exists(PLIST l, void * WILD el) {
  while(l && l->car != el) {
    l= l->cdr;
    l ++;
  }
  return (l != 0);
}

int length(PLIST l) {
  int len = 0;
  while(l) {
    len ++;
    l = l->cdr;
  }
  return len;
}

void main() {

  int i;
  PLIST l = NULL;
  double clk;
  int sum = 0;
  int k;
  TIMESTART(clk);
  for(i=1;i<1000;i++) {
    k = random() % 1000;
    if(length(l) & 1) {
      l = insert(l, (void* WILD)k, k % i);
    } else {
      l = append(l, (void* WILD)k);
    }
  }
  for(i=0;i<10000;i++) {
    k = random() % 1000;
    if(exists(l, (void* WILD)k))
      sum ++;
  }
  TIMESTOP(clk);
  printf("Ran the test %d times in %8.3lfms. Length is %d. Success %d times.\n",
         i, clk / 1000.0, length(l), sizeof(char*), sum);
}



