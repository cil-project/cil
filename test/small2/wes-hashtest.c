/* 
 * Copyright (c) 1999 Cedilla Systems Incorporated.  All Rights Reserved.
 *
 * This software is the confidential and proprietary information of
 * Cedilla Systems Incorporated. ("Confidential Information").  You shall
 * not disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Cedilla Systems.
 * 
 *****************************************************************************/

#include "main.h"
#include "hash.h"
#include "alloc.h"

#ifdef SMALLMEM
#define  BUCKETS_SHIFT 5
#define  BUCKET_SIZE 4
#else
#define  BUCKETS_SHIFT 8
#define  BUCKET_SIZE 8
#endif

#define  NR_HASH_BUCKETS (1 << (BUCKETS_SHIFT))  /* These must be 2^k */


			      /* A Hash entry holds a particular pair (key, 
                               * data)  */
#define EMPTY_ENTRY           0x54854A33

typedef struct HashEntry {
  int key;                    /* The key EMPTY_ENTRY is reserved for empty 
                               * entries  */
  void *data;
} HASH_ENTRY;

			      /* A bucket is a list of clusters of 
                               * HASH_ENTRIES. It behaves like a list of 
                               * entries but it is optimized by allocating 
                               * a cluster of entries at a time */
typedef struct BucketData {
  struct BucketData *next;
  HASH_ENTRY entries[BUCKET_SIZE];
} BUCKET_DATA;


typedef struct {
  int size;
  BUCKET_DATA *data;
} HASH_BUCKET;

unsigned SizeHash(PHASH hash) {
  int i;
  HASH_BUCKET *pBucket = (HASH_BUCKET*)hash;
  unsigned res = 0;
  for(i=0;i<NR_HASH_BUCKETS;i++, pBucket++) {
    res += pBucket->size;
  }
  return res;
}
                              /* A hash table is actually an array of 
                               * NR_HASH_BUCKETS * HASH_BUCKET */

			      /* Converts a hash key to a bucket index */
#define HashKeyToBucket(k, res) { unsigned int _t = (unsigned int)k;\
                                  res = 0;\
                                  for(res = 0;_t;_t >>= BUCKETS_SHIFT) {\
                                       res ^= (_t & (NR_HASH_BUCKETS - 1));\
			        }}


                              /* Keep a list of pre-allocated buckets */
#define BUCKET_CACHE_SIZE      (2 * NR_HASH_BUCKETS)

#ifdef SMALLMEM
#define BUCKET_CACHE_PREALLOC  (BUCKET_CACHE_SIZE >> 2)
#else
#define BUCKET_CACHE_PREALLOC  BUCKET_CACHE_SIZE
#endif

static  BUCKET_DATA *bucketCache[BUCKET_CACHE_SIZE];
static  int nextFreeBucket = 0;


static  BUCKET_DATA *acquireHashBucket(void) {
  if(nextFreeBucket == 0) {
    BUCKET_DATA *buck;
    MALLOC(buck, NULL, sizeof(BUCKET_DATA), BUCKET_DATA*, "hash_bucket_data");
    return buck;
  } else {
    return bucketCache[-- nextFreeBucket];
  }
}

static int releaseHashBucket(BUCKET_DATA *buck) {
  if(nextFreeBucket < BUCKET_CACHE_SIZE) {
    bucketCache[nextFreeBucket ++] = buck;
  } else {
    FREE(buck, "hash_bucket_data");
  }
  return 0;
}

int     preallocateHashes(void) {
  int i;
  nextFreeBucket = 0; /* So that acquire does not steal our buckets  */
  _ASSERT(BUCKET_CACHE_PREALLOC <= BUCKET_CACHE_SIZE);
  for(i=0;i<BUCKET_CACHE_PREALLOC;i++) {
    bucketCache[i] = acquireHashBucket();
  }
  nextFreeBucket = i;
  return 0;
}

int   releaseHashes(void) {
  int i;
  for(i=0;i<nextFreeBucket;i++) {
    FREE(bucketCache[i], "hash_bucket_data");
  }
  nextFreeBucket = 0;
  return 0;
}

/**************** NewHash *******************/
PHASH NewHash(void) {
  /* Allocate a hash */
  PHASH res;
  CALLOC(res, NULL, NR_HASH_BUCKETS, sizeof(HASH_BUCKET), PHASH, "hash_table");
  return (PHASH)res;
}

void FreeHash(PHASH hin) {
  int i;
  HASH_BUCKET *h = (HASH_BUCKET*)hin;
  for(i=0;i<NR_HASH_BUCKETS;i++) {
    HASH_BUCKET *buck = & h[i];
    BUCKET_DATA *bdata = buck->data;
    while(bdata != NULL) {
      BUCKET_DATA *t_bdata = bdata;
      bdata = bdata->next;
      releaseHashBucket(t_bdata);
    }
  }
  FREE(h, "hash_table");
}

typedef enum {SET, LOOKUP, DELETE} HashOper;

static void* ProcessHash(PHASH hin, HASH_KEY key, void *data,
			 int *found, HashOper oper) {
  int bucket_no, i, k;
  BUCKET_DATA *buck = NULL;
  BUCKET_DATA **next = NULL;
  HASH_ENTRY *target = NULL;
  HASH_BUCKET *h = (HASH_BUCKET*)hin;
  
  if(key == EMPTY_ENTRY) { key ++; }
  
  _ASSERT(h);

  HashKeyToBucket(key, bucket_no);	/* Get the bucket number */
  next = & h[bucket_no].data;

  i = BUCKET_SIZE;
  for(k=h[bucket_no].size;k > 0;) { /* Look for the data */
    HASH_ENTRY *e;
    buck = *next;             /* Get the next cluster */ 
    next = &(buck->next);     /* Move one to next cluster */
    e = buck->entries;        /* This is the current entry */
    for(i=0;i < BUCKET_SIZE && k > 0; k--, i++, e++) {
      if(!target && e->key == EMPTY_ENTRY) target = e;
      if(e->key == key) {
	*found = 1;
        switch(oper) {
        case SET: e->data = data; return e->data;
        case LOOKUP: return e->data;
        case DELETE: e->data = NULL; e->key = EMPTY_ENTRY; return NULL; 
	}
      }
    }
    if(k == 0)  /* Not in the bucket, hence not in table */
      break;
    _ASSERT(i == BUCKET_SIZE);
  }
  _ASSERT(k == 0);
  *found = 0;		      /* Here if not found */
  if(oper != SET) {
    return NULL;
  }
  if(! target) {
			      /* Must create a new entry */
    if(i == BUCKET_SIZE) {		     
      if(! next) {
        next = &(h[bucket_no].data);
      }
      _ASSERT(*next == NULL);
      buck = acquireHashBucket();
      *next = buck;
      buck->next = NULL;
      i = 0;
    }
    target = &buck->entries[i];
    h[bucket_no].size ++;
  }
  target->key = key;
  target->data = data;
  return NULL;
}

			      /* Lookup a hash key. Put the result in *data */
int HashLookup(PHASH h, HASH_KEY key, void **data) {
  int found;
  *data = ProcessHash(h, key, NULL, &found, LOOKUP);
  return found;
}

			      /* Extend the hash. If the data already exists 
                               * then replace it*/
int AddToHash(PHASH h, HASH_KEY key, void* data) {
  int found;
  ProcessHash(h, key, data, &found, SET);
  return found;
}

int DeleteFromHash(PHASH h, HASH_KEY key) {
  int found;
  ProcessHash(h, key, NULL, &found, DELETE);
  return 0;
}

int MapHash(PHASH h, void* (*f)(HASH_KEY, void*, UPOINT), UPOINT closure) {
  int i;
  HASH_BUCKET *pBucket = (HASH_BUCKET*)h;

  for(i=0;i<NR_HASH_BUCKETS;i++, pBucket ++) {
    int sz = pBucket->size;
    BUCKET_DATA *pData = pBucket->data;
    HASH_ENTRY *pEntry = pData->entries;
    int k = 0;
    for(;sz > 0;sz --, k++, pEntry++) {
      if(k == BUCKET_SIZE) {
        k = 0;
        pData  = pData->next;
        pEntry = pData->entries;
      }
      if(pEntry->key == EMPTY_ENTRY)
        continue;
      pEntry->data = (*f)(pEntry->key, pEntry->data, closure);
    }
  }
  return 0;
}








/* Some globals that PCC needs */
int error_level, anerror;
void myexit(int n) {
  exit(n);
}
#ifdef _MSVC
#define random rand
#else
/* extern int random(void); -- Weimer: not needed! */
#endif
int __mmId;
int debugMM;
int debug;




int main() {
  /* Test hash tables */
  PHASH h = NewHash();
  int i;
  double clk;
  int count = 0;
  int sz;
  
  /* Add and delete random numbers from the hash table */
  TIMESTART(clk);
  for(i=0;i<500000;i++) {
    int k = random() & 0xFFFFL;
    AddToHash(h, k, (void*)k);
  }
  for(i=0;i<500000;i++) {
    int k = random() & 0xFFFFL;
    void *data = NULL;
    if(HashLookup(h, k, & data)) {
      count ++;
    }
  }
  sz = SizeHash(h);
  FreeHash(h);
  TIMESTOP(clk);
  printf( "Hash has %d elements. Found %d times\n",
          sz, count);
  printf("Run hashtest in %8.3lfms\n", clk / 1000.0);
  printf("Hello\n");
  exit (0);
}


