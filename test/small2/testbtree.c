/***********************************************************************\
|									|
|	B+tree function tests						|
|									|
|									|
|	Jan Jannink	created 12/22/94	revised 1/30/95		|
|									|
\***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "btree.h"

int main(void)
{
  Tree	*Bplus;
  Nptr	keyNode;
  int	i, j;

  Bplus = initBtree(ARRAY_SIZE, NODE_SIZE / sizeof(Entry), compareKeys);
/*  insert(Bplus,17);
  insert(Bplus,16);
  insert(Bplus,15);
  insert(Bplus,14);
  insert(Bplus,13);
  insert(Bplus,12);
  insert(Bplus,11);
  insert(Bplus,10);
  insert(Bplus,9);
  insert(Bplus,8);
  insert(Bplus,7);
  insert(Bplus,6);
  insert(Bplus,5);
  insert(Bplus,4);
  insert(Bplus,3);
  insert(Bplus,2);
  insert(Bplus,1);
  delete(Bplus,1);
  delete(Bplus,2);
  delete(Bplus,3);
  delete(Bplus,4);
  delete(Bplus,5);
  delete(Bplus,6);
  delete(Bplus,7);
  delete(Bplus,8);
  delete(Bplus,9);
  delete(Bplus,10);
  delete(Bplus,11);
  delete(Bplus,12);
  delete(Bplus,13);
  delete(Bplus,14);
  delete(Bplus,15);
  delete(Bplus,16);
  delete(Bplus,17); */
  for (i = 0; i < 2048; i++) {
    j = rand() >> 3 & 255;
    if (search(Bplus, j) == Bplus->tree - 1) {
      insert(Bplus, j);
      fprintf(stderr, "XXX %d, insert %d XXX\n", i, j);
    }
    else {
      delete(Bplus, j);
      fprintf(stderr, "XXX %d, delete %d XXX\n", i, j);
    }
    if (i > 2000)
      listAllBtreeValues(Bplus);
  }
  for (i = 0; i < 256; i++)
    (void) search(Bplus, i);
  listAllBtreeValues(Bplus);
  freeBtree(Bplus);

  return 1;
}

