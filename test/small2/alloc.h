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

#ifndef  __ALLOC_H
#define __ALLOC_H

int    initAlloc(UL size);
void   freePool(void);

void* alloc(UL size);
char* myStrdup(char*);
void  gc(void* mrk);

/* Query functions */
extern UL    maxPoolLen;
extern UL    poolLen;

UL    poolLeft(void);
UL    poolUsedSince(void* mrk);


/* Return a marker in the pool for future deallocation */
extern  void* apoint;
#define MARK apoint
/* Perform alignment */

#define FP_TO_U(fp) ((UPOINT)(fp))
#define U_TO_FP(ul) ((void*)(ul))


#define POINT_DIFF(p1, p2) ((SPOINT)(FP_TO_U(p1) - FP_TO_U(p2)))

#endif /* _ALLOC_H */
