typedef struct { int counter; } atomic_t;

static __inline__ void atomic_add(int i, volatile atomic_t *v)
{
        __asm__ __volatile__(
		""  "addl %1,%0"
		:"=m" ((*(volatile struct { int a[100]; } *) v ) )
		:"ir" (i), "m" ((*(volatile struct { int a[100]; } *) v ) ));
}
