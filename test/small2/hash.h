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

typedef int HASH_KEY;
typedef void *PHASH;          /* Poor's man abstraction */




PHASH NewHash(void);
void  FreeHash(PHASH);

			      /* The following functions return TRUE if the 
                               * particular data was already in the hash */
int   HashLookup(PHASH, HASH_KEY, void** data);
			      /* If data already exists, then replace it */
int   AddToHash(PHASH, HASH_KEY, void*);

                              /* Nothing happens if the key does not exits */
int   DeleteFromHash(PHASH, HASH_KEY);

                              /* Maps a function to a hash table. The last 
                               * element is a closure. The data is 
                               * overwritten but not placed into another 
                               * bucket ! */
int   MapHash(PHASH, void* (*)(HASH_KEY, void*, UPOINT), UPOINT);

                              /* Returns the number of elements in the table */
unsigned int   SizeHash(PHASH);
                              /* Preallocates some hashes */
int   preallocateHashes(void);
                              /* And release them */
int   releaseHashes(void);
