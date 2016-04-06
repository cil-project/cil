/* Repeating a prototype should be harmless. */

extern void f(void);
extern inline __attribute__((always_inline)) void g()
{
	f();
}

extern inline __attribute__((always_inline)) void g();
void v(void)
{
	g();
}
