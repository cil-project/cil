extern __inline   double     atan   ( double  __x)  	{
  register  double  __result;
  __asm __volatile__ (
                      "fld1; fpatan"
                       : "=t" (__result) :
                       "0" (__x)
                       : "st(1)"  );
  return __result;
}

extern __inline   float      atanf    ( float  __x)  	{
  register  float  __result;
  __asm __volatile__ ( "fld1; fpatan"
                       : "=t" (__result)
                       :     "0" (__x)
                       : "st(1)"  );
  return __result;
}

extern __inline   long double      atanl    ( long double  __x) {
  register  long double  __result;
  __asm __volatile__ (    "fld1; fpatan"
                          : "=t" (__result)
                          :     "0" (__x)
                          : "st(1)"  );
  return __result;
}  
