

#include "main.h"
#include "redblack.h"

#define Red   0
#define Black 1


static RBNode *leftRotate(RBNode *r) {
  RBNode *t;
  _ASSERT(r->right);
  t        = r->right;
  r->right = t->left; if(t->left) {t->left->parent = r; }
  
  if(r->parent) {
    if(r->parent->left == r) {
      r->parent->left = t;
    } else {
      r->parent->right = t;
    }
  }
  t->parent = r->parent;
  t->left  = r; r->parent = t;
  return     t;  // like r = t  
}

static RBNode *rightRotate(RBNode *r) {
  RBNode *t;
  _ASSERT(r->left);
  t         = r->left;
  r->left   = t->right; if(t->right) { t->right->parent = r; }
  
  if(r->parent) {
    if(r->parent->left == r) {
      r->parent->left = t;
    } else {
      r->parent->right = t;
    }
  }
  t->parent = r->parent;
  t->right  = r; r->parent = t;
  return     t;
}

static RBNode * fixupRB(RBNode *x);
#ifdef _DEBUG
/* Check the tree and return the black height */
static int checkRBTreeRec(RBNode *tree, U32 minKey, U32 maxKey) {
  int bhl, bhr;
  if(! tree) return 1;
  _ASSERT((! tree->left || tree->left->parent == tree) &&
          (! tree->right || tree->right->parent == tree));
  _ASSERT(tree->key >= minKey && tree->key <= maxKey);
  _ASSERT(tree->color == Red || tree->color == Black);
  _ASSERT(tree->color == Black ||
          ((!tree->left || tree->left->color == Black) &&
           (!tree->right || tree->right->color == Black)));
  bhl = checkRBTreeRec(tree->left, minKey, tree->key);
  bhr = checkRBTreeRec(tree->right, tree->key + 1, maxKey);
  _ASSERT(bhl == bhr);
  return tree->color == Black ? bhl + 1 : bhl;
}

static int checkRBTree(RBNode *tree) {
  _ASSERT(tree->color == Black);
  checkRBTreeRec(tree, 0, (U32)(-1));
  return 1;
}

static int printRBIndent(U32 address) {
  if(address) {
    printf(" ");
    printRBIndent(address >> 1);
    printf("%d", address & 1);
  }
  return 1;
}

static int printRBTree(RBNode *tree, U32 address) {
  printRBIndent(address);
  if(tree) {
    printf(":%s - 0x%08lx\n",
           tree->color == Red ? "Red  " : "Black", tree->key);
    printRBTree(tree->left, address << 1);
    printRBTree(tree->right, (address << 1) + 1);
  } else {
    printf(":NIL\n");
  }
  return 1;
}
#endif

char* insertRB(RBNode **tree, U32 key, int datalen) {
  RBNode *x, *t;
  CALLOC(x, NULL, sizeof(RBNode) + datalen, 1, RBNode*, "RBNode");
  x->left = NULL;
  x->right = NULL;
  x->parent = NULL;
  x->color = Red;
  x->key   = key;

  // Now insert as if it were a simple binary search tree
  {
    RBNode **p = tree;
    RBNode *parent = NULL;
    while(*p) {               /* We have not reached a NIL */
      parent = *p;
      if(key <= (*p)->key) {
        p = & (*p)->left;
      } else {
        p = & (*p)->right;
      }
    }
    // Now *p = NIL
    *p = x; x->parent = parent;
  }
  t = fixupRB(x);
  if(t->parent == NULL) {
    *tree = t;
  }
  _ASSERT(*tree);
  (*tree)->color = Black;
  //  IFDEBUG(printf("Tree after insert of key=0x%08lx is\n", key);
  //        printRBTree(*tree, 1););
  IFDEBUG(checkRBTree(*tree));
  return & x->data[0];        /* Return the allocated node */
}


static RBNode * fixupRB(RBNode *x) {
  // Now fixup. We know that x is always RED. The root is always Black
  while(x->parent && x->parent->color == Red) {
    RBNode *par  = x->parent;
    RBNode *gpar = par->parent;
    RBNode *uncle;
    _ASSERT(x->color == Red);
    _ASSERT(gpar);  // the root is always black, so we must have a grand par
    _ASSERT(gpar->color == Black);
    if(par == gpar->left) {
      uncle = gpar->right;
      if(uncle && uncle->color == Red) {
      Case1:
        par->color   = Black;
        uncle->color = Black;
        gpar->color  = Red;
        x = gpar;
        continue;
      } else {
        _ASSERT(!uncle || uncle->color == Black);
        if(x == par->right) {
          uncle = x;
          leftRotate(par);
          x   = par;
          par = uncle;
        }
        _ASSERT(x == par->left);
        rightRotate(gpar);
        par->color = Black;
        gpar->color = Red;
        return par;
      }
    } else {
      uncle = gpar->left;
      _ASSERT(par == gpar->right);
      if(uncle && uncle->color == Red) {
        goto Case1;
      } else {
        _ASSERT(! uncle || uncle->color == Black);
        if(x == par->left) {
          uncle = x;
          rightRotate(par);
          x   = par;
          par = uncle;
        }
        _ASSERT(x == par->right);
        leftRotate(gpar);
        par->color = Black;
        gpar->color = Red;
        return par;
      }
    }
  }
  return x;
}

char* findRB(RBNode *tree, U32 key) {
  while(tree) {
    if(tree->key == key)
      return & tree->data[0];
    if(key < tree->key)
      tree = tree->left;
    else
      tree = tree->right;
  }
  return NULL;
}

int freeRB(RBNode *tree, int (*freeData)(U32 key, char *data)) {
  if(! tree) return 1;
  freeRB(tree->left, freeData);
  freeRB(tree->right, freeData);
  // Now free the node
  if(freeData) {
    (*freeData)(tree->key, & tree->data[0]);
  }
  free(tree);
  return 1;
}


#include "alloc.h"

/* Some globals that PCC needs */
int error_level, anerror;
void myexit(int n) {
  exit(n);
}
#ifdef _MSVC
#define random rand
#else
/* weimer: not needed: extern int random(void); */
#endif
int __mmId;
int debugMM;
int debug;


#define DATASIZE 16   // This is the size of the data that is reserved in
                      // each node

int main() {
  /* Test hash tables */
  RBNode *t = NULL;
  int i;
  double clk;
  int count = 0;
  int sz;
  
  /* Add and delete random numbers from the hash table */
  TIMESTART(clk);
  for(i=0;i<500000;i++) {
    int k = random() & 0xFFFFL;
    insertRB(& t, k, DATASIZE);
  }
  for(i=0;i<500000;i++) {
    int k = random() & 0xFFFFL;
    void *data = NULL;
    if(findRB(t, k)) {
      count ++;
    }
  }
  sz = 0;
  FORALLRBNODES(t, { sz ++; });
  freeRB(t, NULL);
  TIMESTOP(clk);
  printf("Hash has %d elements. Found %d times\n",
          sz, count);
  printf("Run hashtest in %8.3lfms\n", clk / 1000.0);
  printf("Hello\n");
  exit (0);
}


