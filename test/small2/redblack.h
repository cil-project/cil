/* An implementation of red-black trees */
/* Supports insertion, lookup and scanning in the increasing order of the 
 * key */

#ifndef __REDBLACK_H
#define __REDBLACK_H

typedef struct rbNode {
  struct rbNode *left, *right, *parent;
  U32    key;
  U8     color;
  char data[1];
} RBNode;

/*** KEYS are compared using unsigned comparisons ****/

/* Creates a new RB node. The node has room for some data but nothing is put 
 * in there. The pointer to the data is returned. Start with NULL as an 
 * empty tree */
char* insertRB(RBNode **tree, U32 key, int datalen);


/* Finds a node. Returns a pointer to the data */
char* findRB(RBNode *tree, U32 key);

/* Pass freeData=NULL if the data does not contain pointers that need to be 
 * deallocated */
int freeRB(RBNode *tree, int (*freeData)(U32 key, char *data));

// A non-recursive scanner for RB trees
#define FORALLRBNODES(tree, donode) {\
 if(tree) {\
  DoLeftChildren:\
    while(tree->left) {\
      tree = tree->left;\
    }\
   DoNode:\
    /* No left child, or have done all the left descendants*/\
    donode;\
    if(tree->right) {\
      tree = tree->right;\
      goto DoLeftChildren;\
    }\
    /* No right child and we have done all the left descendants*/\
    while(tree->parent && tree->parent->right == tree)\
      tree = tree->parent;\
    if(tree->parent) {\
      tree = tree->parent;\
      goto DoNode;\
    }\
 }\
}

#endif /* __REDBLACK_H */
