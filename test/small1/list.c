#include <stdlib.h>
#include <stdio.h>

typedef struct list {
  void*  car;
  struct list *cdr;
} LIST;


LIST *prepend(LIST *l, void* el) {
  LIST *n = (LIST*)malloc(sizeof(LIST));
  n->car = el;
  n->cdr = l;
  return n;
}


LIST *append(LIST *l, void *el) {
  LIST *parent = 0;
  LIST *n = l;
  while(n) {
    parent = n;
    n = n->cdr; 
  }
  n = (LIST*)malloc(sizeof(LIST));
  n->car = el;
  n->cdr = 0;
  if(parent) {
    parent->cdr = n;
    return l;
  } else {
    return n;
  }
}


LIST *insert(LIST *l, void *el, int pos) {
  LIST * n = l;
  LIST * t;
  if(l) {
    while(n->cdr && pos > 0) {
      n = n->cdr;
    }
  }
  t = (LIST*)malloc(sizeof(LIST));
  if(! l) {
    t->cdr = NULL;
    return t;
  } else {
    t->cdr = n->cdr;
    n->cdr = t;
    return l;
  }
}

int exists(LIST *l, void *el) {
  while(l && l->car != el) {
    l= l->cdr;
  }
  return (l != 0);
}

int length(LIST *l) {
  int len = 0;
  while(l) {
    len ++;
    l = l->cdr;
  }
  return len;
}

void main() {

  int i;
  LIST *l = NULL;
  double clk;
  int sum = 0;
  int k;
  TIMESTART(clk);
  for(i=1;i<1000;i++) {
    k = random() % 1000;
    l = insert(l, (void*)k, k % i);
  }
  for(i=0;i<10000;i++) {
    k = random() % 1000;
    if(exists(l, (void*)k))
      sum ++;
  }
  TIMESTOP(clk);
  printf("Ran the test %d times in %8.3lfms. Length is %d. Success %d times.\n",
         i, clk / 1000.0, length(l), sizeof(char*), sum);
}
