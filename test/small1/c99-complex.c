#include <stdio.h>
#include <complex.h>
#include <tgmath.h>
#include "testharness.h"

void forlong() {
    long double complex z1 = 1.0il + 1;
    printf("I * I = %.1Lf%+.1Lfi\n", creal(z1), cimag(z1));

    long double complex z2 = pow(_Complex_I, 2); // imaginary unit squared
    printf("pow(I, 2) = %.1Lf%+.1Lfi\n", creal(z2), cimag(z2));

    double long PI = acos(-1);
    double long complex z3 = exp(I * PI); // Euler's formula
    printf("exp(I*PI) = %.1Lf%+.1Lfi\n", creal(z3), cimag(z3));

    double long complex z4 = 1+2*I, z5 = 1-2*I; // conjugates
    printf("(1+2i)*(1-2i) = %.Lf%+.Lfi\n", creal(z4*z5), cimag(z4*z5));
}

int main(void)
{
    double complex x0 = 1.0i + 17;
    double complex x1 = 1.0iF + 0.5;
    double complex x00 = 1.0Fi + 0.5;

    if(sizeof(double complex) != sizeof(1.0iF + 0.5)) {
        E(1);
    }

    double d = creal(x1);
    double i = cimag(x1);

    double j = __imag__(1.0if);

    if(d != 0.5)
        E(2);

    if(i != 1.0 || j != 1.0)
        E(3);

    double complex z1 = 1.0iF + 1;
    printf("I * I = %.1f%+.1fi\n", creal(z1), cimag(z1));

    double complex z2 = pow(_Complex_I, 2); // imaginary unit squared
    printf("pow(I, 2) = %.1f%+.1fi\n", creal(z2), cimag(z2));

    double PI = acos(-1);
    double complex z3 = exp(I * PI); // Euler's formula
    printf("exp(I*PI) = %.1f%+.1fi\n", creal(z3), cimag(z3));

    double complex z4 = 1+2*I, z5 = 1-2*I; // conjugates
    printf("(1+2i)*(1-2i) = %.1f%+.1fi\n", creal(z4*z5), cimag(z4*z5));

    forlong();
    return 0;
}

void parsedebug() {
    __real__ (1.0iF);
    __real__ (2);
    0 + __real__(2);
    __real__(2) + 0;
    (sizeof (__real__ (1.0iF) + __real__ (2)) > sizeof (double) && __builtin_classify_type (__real__ (1.0iF) + __real__ (2)) == 8);


   (__extension__ ((sizeof (__real__ (1.0iF) + __real__ (2)) > sizeof (double) && __builtin_classify_type (__real__ (

   1.0iF

   ) + __real__ (

   2

   )) == 8) ? ((__builtin_classify_type ((

   1.0iF
   ) + (

   2

   )) != 9) ? (__typeof ((__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(

   1.0iF

   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 1))))) 0)) 0))) 0 + (__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   2
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 1))))) 0)) 0))) 0)) powl (
   1.0iF
   ,
   2
   ) : (__typeof ((__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   1.0iF
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 1))))) 0)) 0))) 0 + (__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   2
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 1))))) 0)) 0))) 0)) cpowl (
   1.0iF
   ,
   2
   )) : (sizeof (+__real__ (
   1.0iF
   )) == sizeof (double) || sizeof (+__real__ (
   2
   )) == sizeof (double) || __builtin_classify_type (__real__ (
   1.0iF
   )) != 8 || __builtin_classify_type (__real__ (
   2
   )) != 8) ? ((__builtin_classify_type ((
   1.0iF
   ) + (
   2
   )) != 9) ? (__typeof ((__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   1.0iF
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 1))))) 0)) 0))) 0 + (__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   2
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 1))))) 0)) 0))) 0)) pow (
   1.0iF
   ,
   2
   ) : (__typeof ((__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   1.0iF
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 1))))) 0)) 0))) 0 + (__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   2
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 1))))) 0)) 0))) 0)) cpow (
   1.0iF
   ,
   2
   )) : ((__builtin_classify_type ((
   1.0iF
   ) + (
   2
   )) != 9) ? (__typeof ((__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   1.0iF
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 1))))) 0)) 0))) 0 + (__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   2
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 1))))) 0)) 0))) 0)) powf (
   1.0iF
   ,
   2
   ) : (__typeof ((__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   1.0iF
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   1.0iF
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   1.0iF
   ))) 0)) == 1))))) 0)) 0))) 0 + (__typeof__ (*(0 ? (__typeof__ (0 ? (__typeof__ ((__typeof__ (+(
   2
   ))) 0) *) 0 : (void *) (!((__builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 8))))) 0 : (__typeof__ (0 ? (__typeof__ (0 ? (double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 1))))) 0 : (__typeof__ (0 ? (_Complex double *) 0 : (void *) (!((__builtin_classify_type ((__typeof__ (+(
   2
   ))) 0) == 9 && __builtin_classify_type (__real__ ((__typeof__ (+(
   2
   ))) 0)) == 1))))) 0)) 0))) 0)) cpowf (
   1.0iF
   ,
   2
   ))))
                ;
}
