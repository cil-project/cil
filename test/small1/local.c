/* xlbfun.c - xlisp basic built-in functions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "local.h"

NODE*** xlstack;


/* xbquote - back quote function */
NODE *xbquote(NODE *args)
{
  NODE *expr,*val;  
  val = bquote1(expr);
  return (val);
}

/* bquote1 - back quote helper function */
NODE *bquote1(NODE *expr)
{
  NODE ***oldstk;
  rplacd(expr,bquote1(cdr(expr)));
  xlstack = oldstk;
  return (expr);
}

