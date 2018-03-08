/* A silly little test of inlining in GNU C.
 *
 * We declare/define a bunch of "inline" functions, then do some introspection
 * to assert that they were/weren't inlined as we expected.
 *
 * This isn't a standard-conformance test, because it makes a big assumption:
 * that if inlining *can* be done, it *will* be done. That is usually what a
 * compiler is going for, though, for the small inline functions we have here.
 *
 * It's x86-only for now, because there are a few inline assembly snippets.
 * These could be #ifdef'd to support other architectures.
 *
 * It's also very specific to ELF systems (for the introspection stuff).
 * Link with --export-dynamic -ldl. I have been doing this:
 *
 * cc -save-temps -Wl,-export-dynamic -g -std=c99 -O2 -o the-test the-test.c -ldl
 *
 * The semantics of "inline" differ in GCC's gnu89 mode. I have provided
 * alternative tests for this case, probed via the  __GNUC_STDC_INLINE__ macro.
 *
 * NOTE that we use the GNUism of "statement expressions". This could be
 * eliminated by uglifying the assertions somewhat (making each entire
 * assertion into a macro). Note that using an inline helper function is
 * not a good idea here, though it would be if we were not testing inlining.
 *
 * To control optimisation, I am using a mixture of GCC pragmas and the
 * "optnone" attribute that clang supports. These could be macroised to
 * allow testing other compilers, assuming they provide analogous features.
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <err.h>
#include <dlfcn.h>

/* We will need to get our own program counter, to ask questions about it. */
#ifdef __x86_64__
#define GET_PC \
	({ uintptr_t addr; \
	    __asm__ volatile (".byte 0xe8   # callq \n\
                           .byte 0x0    # 0 \n\
                           .byte 0x0    # \n\
                           .byte 0x0    # \n\
                           .byte 0x0    # \n\
                           pop %0" : "=r"(addr) ); \
	(const void*)addr; })
#else
#error "Unsupported architecture"
#endif

/* We need to define a test for whether we've been inlined.
 *
 * Complication: sometimes gcc will "outline" part of an inlined function,
 * in which case it will end up in some local symbol area. This is what we
 * suppose has happened if dladdr gives us !ret. Rationale: if it had simply
 * not been inlined, it'd be under a global symbol. */
#define AM_INLINED \
	({ Dl_info d; int ret = dladdr(GET_PC, &d); !(ret && d.dli_sname) || (ret && d.dli_sname && 0 != strcmp(d.dli_sname, __func__)); })

/* We need to test for whether an out-of-line instance exists. */
#define OOL_EXISTS  (dlsym(RTLD_DEFAULT, __func__))

/* Our test cases consist of two decls (prototypes) and one definition.
 * We use two prototypes because the C standard notes that it matters
 * whether *all* prototypes carry a particular qualifier (extern) or
 * just some.
 *
 * We don't currently test "static inline".
 * 
 * So...
 * we emit three prototypes in each case;
 * prototypes can be annotated 3 ways: { inline, <nothing>, extern inline }.
 * We have 27 cases.
 *
 * For each case we create an unoptimised "n" caller and an
 * optimised "o" caller. We need a different body for the
 * optimised (a.k.a. maybe-inlined) callees, to assert the
 * different things that we expect to hold.
 *
 * We also emit an out-of-line copy in cases where we think that the compiler
 * will *not* generate one by itself. If we are wrong about this, we will
 * "fail the assertion" by seeing a link error.
 *
 * There are two different bodies for the out-of-line copies: one where we
 * expect it never to be called, i.e. because inline was done; in this case
 * the out-of-line copy aborts. In the other case, it exits successfully.
 *
 * PROBLEM: we run our tests as constructors. When they run, on my system at
 * least, the SIGABRT handler has not yet been installed. So don't abort;
 * exit nonzero. FIXME.
 */
#define EMIT_OOL(sym) \
__asm__( \
		".pushsection .text.ool,\"ax\",@progbits\n" \
		".globl " #sym "\n" \
		".type " #sym ", @function\n" \
		"" #sym ":\n" \
		"    mov    $0x29,%eax\n" \
		"    retq\n" \
);
#define EMIT_OOL_NOTCALLED(sym) \
__asm__( \
		".pushsection .text.ool,\"ax\",@progbits\n" \
		".globl " #sym "\n" \
		".type " #sym ", @function\n" \
		"" #sym ":\n" \
		"    callq    abort@plt\n" \
);

#ifdef __clang__
#define NO_OPT __attribute__((optnone))
#else
#define NO_OPT
#endif

#ifdef __GNUC_STDC_INLINE__

/* inline, inline, inline, unoptimised.
 * We expect the inlining not to be done,
 * and an undefined reference emitted. */
#pragma GCC optimize ("O0")
inline int f1n(void) NO_OPT;
inline int f1n(void) NO_OPT;
inline int f1n(void)
{
	// OOL is defined separately; this def should not create it
	assert(0 && "this inline def should not be used");
}
#pragma GCC optimize ("O2")
#pragma GCC optimize ("no-optimize-sibling-calls")
int c1n(void) { return f1n(); }
/* Run the test from a constructor. This is so that we can
 * macroise test cases in a self-contained fashion (see below). */
static void init1n(void) __attribute__((constructor)) NO_OPT;
static void init1n(void) { c1n(); }
#pragma GCC optimize ("O2")
// define the OOL copy -- if the compiler also generates one, we'll get a link error
EMIT_OOL(f1n)

/* inline, inline, inline, optimised.
 * We expect inlining to be done.
 * If an out-of-line copy is emitted, it
 * should be local. However, we may get
 * a reference to an external def, so
 * the programmer should create one. */
inline int f1o(void);
inline int f1o(void);
inline int f1o(void)
{
	assert(AM_INLINED);
	// OOL is defined separately; this def should not create it
	return 42;
}
// define the OOL copy -- if the compiler also generates one, we'll get a link error
EMIT_OOL(f1o)
// disable tail-call opts for the c1o, as they may trump inlining (get callq <f1o> in init)
#pragma GCC optimize ("O2")
#pragma GCC optimize ("no-optimize-sibling-calls")
int c1o(void) { return f1o(); }
#pragma GCC optimize ("O0")
static void init1o(void) __attribute__((constructor));
static void init1o(void) { c1o(); }
#pragma GCC optimize ("O2")

#endif
/* We handle the GNU-style cases below */

/* For later assertions, we actually want to warn and continue. */
#undef assert
#define assert(cond) \
	do { \
		if (!cond) warnx("`Assertion' failed: `%s' in %s at %s, line %d", #cond, __func__, __FILE__, __LINE__); \
	} while (0)

#ifdef __GNUC_STDC_INLINE__
/* <nothing>, inline, inline, unoptimised.
 * We expect the inlining not to be done,
 * and a defined symbol emitted. */
#pragma GCC optimize ("O0")
int f2n(void) NO_OPT;
inline int f2n(void) NO_OPT;
inline int f2n(void)
{
	assert(!AM_INLINED);
	assert(OOL_EXISTS);
	return 42;
}
#pragma GCC optimize ("O2")
#pragma GCC optimize ("no-optimize-sibling-calls")
int c2n(void) { return f2n(); }
#pragma GCC optimize ("O0")
static void init2n(void) __attribute__((constructor)) NO_OPT;
static void init2n(void) { c2n(); }
#pragma GCC optimize ("O2")

/* <nothing>, inline, inline, optimised.
 * We expect inlining to be done,
 * and a defined symbol to be emitted. */
int f2o(void);
inline int f2o(void);
inline int f2o(void)
{
	assert(OOL_EXISTS);
	assert(AM_INLINED);
	return 42;
}
#pragma GCC optimize ("no-optimize-sibling-calls")
int c2o(void) { return f2o(); }
#pragma GCC optimize ("O0")
static void init2o(void) __attribute__((constructor)) NO_OPT;
static void init2o(void) { c2o(); }
#pragma GCC optimize ("O2")

#endif

/* This is getting boring, so define some macros.
 * In fact, all the cases from here on are boring.
 * Only <inline, inline, inline> is interesting. */

#define CASEo( idx, q1, q2, q3, asserts ) \
q1 int f ## idx ## o(void); \
q2 int f ## idx ## o(void); \
q3 int f ## idx ## o(void) \
{ \
	asserts; \
	return 42; \
} \
_Pragma("GCC optimize (\"no-optimize-sibling-calls\")") \
int c ## idx ## o(void) { return f ## idx ## o(); } \
_Pragma("GCC optimize (\"O0\")") \
static void init ## idx ## o(void) __attribute__((constructor)) NO_OPT; \
static void init ## idx ## o(void) { c ## idx ## o(); } \
_Pragma("GCC optimize (\"optimize-sibling-calls\")")

#define CASEn( idx, q1, q2, q3, asserts ) \
_Pragma("GCC optimize (\"O0\")") \
q1 int f ## idx ## n(void) NO_OPT; \
q2 int f ## idx ## n(void) NO_OPT; \
q3 int f ## idx ## n(void) \
{ \
	asserts; \
	return 42; \
} \
int c ## idx ## n(void) { return f ## idx ## n(); } \
_Pragma("GCC optimize (\"O0\")") \
static void init ## idx ## n(void) __attribute__((constructor)) NO_OPT; \
static void init ## idx ## n(void) { c ## idx ## n(); } \
_Pragma("GCC optimize (\"O2\")")

#ifdef __GNUC_STDC_INLINE__
// we *would* have written something like this
// CASEn(1, inline, inline, inline, assert(!OOL_EXISTS); assert(!AM_INLINED); /* UND ref created! */ )
// CASEo(1, inline, inline, inline, assert(!OOL_EXISTS); assert(AM_INLINED);)
// CASEn(2, , inline, inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
// CASEo(2, , inline, inline, assert(OOL_EXISTS); assert(AM_INLINED);)
#else
// gnu89-style, these are a bit different
CASEn(1, inline, inline, inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(1, inline, inline, inline, assert(OOL_EXISTS); assert(AM_INLINED);)
CASEn(2, , inline, inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(2, , inline, inline, assert(OOL_EXISTS); assert(AM_INLINED);)
// remember that in our CASE macros, the third qualifier slot (q3) goes on the definition
// so things get interesting when that bumps around to "extern inline" (case 19)
#endif

CASEn(3, extern inline, inline, inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(3, extern inline, inline, inline, assert(OOL_EXISTS); assert(AM_INLINED);)

// bump the middle decl to <nothing>

CASEn(4, inline, , inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(4, inline, , inline, assert(OOL_EXISTS); assert(AM_INLINED); )

CASEn(5, , , inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(5, , , inline, assert(OOL_EXISTS); assert(AM_INLINED); )

CASEn(6, extern inline, , , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(6, extern inline, , , assert(OOL_EXISTS); assert(AM_INLINED); )

// bump the middle decl to extern inline
CASEn(7, inline, extern inline, inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(7, inline, extern inline, inline, assert(OOL_EXISTS); assert(AM_INLINED); )

CASEn(8, , extern inline, inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(8, , extern inline, inline, assert(OOL_EXISTS); assert(AM_INLINED); )

CASEn(9, extern inline, extern inline, inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(9, extern inline, extern inline, inline, assert(OOL_EXISTS); assert(AM_INLINED); )

// bump the top decl to <nothing> and reset the others

CASEn(10, inline, inline, , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(10, inline, inline, , assert(OOL_EXISTS); assert(AM_INLINED); )

CASEn(11, , inline, , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(11, , inline, , assert(OOL_EXISTS); assert(AM_INLINED); )

CASEn(12, extern inline, inline, , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(12, extern inline, inline, , assert(OOL_EXISTS); assert(AM_INLINED); )

// bump the middle decl to <nothing>

CASEn(13, inline, , , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(13, inline, , , assert(OOL_EXISTS); assert(AM_INLINED);  )

CASEn(14, , , , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(14, , , , assert(OOL_EXISTS); assert(!AM_INLINED); )

CASEn(15, extern inline, , , assert(OOL_EXISTS); assert(!AM_INLINED);  )
CASEo(15, extern inline, , , assert(OOL_EXISTS); assert(AM_INLINED);  )

// bump the middle decl to 'extern inline'

CASEn(16, inline, extern inline, , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(16, inline, extern inline, , assert(OOL_EXISTS); assert(AM_INLINED); )

CASEn(17, , extern inline, , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(17, , extern inline, , assert(OOL_EXISTS); assert(AM_INLINED); )

CASEn(18, extern inline, extern inline, , assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(18, extern inline, extern inline, , assert(OOL_EXISTS); assert(AM_INLINED); )

// bump the top decl (i.e. the defn) to "extern inline" and reset the others

#ifdef __GNUC_STDC_INLINE__
CASEn(19, inline, inline, extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(19, inline, inline, extern inline, assert(OOL_EXISTS); assert(AM_INLINED); )
#else
/* "The definition is only used for inlining." So we can emit an OOL one.
 * Except that if we declare it also as "[regular] inline", that gets undone. */
CASEn(19, inline, inline, extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(19, inline, inline, extern inline, assert(AM_INLINED);  )
#endif

#ifdef __GNUC_STDC_INLINE__
CASEn(20, , inline, extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(20, , inline, extern inline, assert(OOL_EXISTS); assert(AM_INLINED); )
#else
/* "The definition is only used for inlining." So we can emit an OOL one.
 * Except that if we declare it also as "[regular] inline", that gets undone. */
CASEn(20, , inline, extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(20, , inline, extern inline, assert(OOL_EXISTS); assert(AM_INLINED);  )
#endif

#ifdef __GNUC_STDC_INLINE__
CASEn(21, inline, , extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(21, inline, , extern inline, assert(OOL_EXISTS); assert(AM_INLINED); )
#else
/* "The definition is only used for inlining." So we can emit an OOL one.
 * Except that if we declare it also as "[regular] inline", that gets undone. */
CASEn(21, inline, , extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(21, inline, , extern inline, assert(OOL_EXISTS); assert(AM_INLINED);  )
#endif

// bump the middle decl to <nothing>

#ifdef __GNUC_STDC_INLINE__
CASEn(22, inline, , extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(22, inline, , extern inline, assert(OOL_EXISTS); assert(AM_INLINED);  )
#else
/* "The definition is only used for inlining." So we can emit an OOL one.
 * Except that if we declare it also as "[regular] inline", that gets undone. */
CASEn(22, inline, , extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(22, inline, , extern inline, assert(OOL_EXISTS); assert(AM_INLINED);  )
#endif

#ifdef __GNUC_STDC_INLINE__
CASEn(23, , , extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(23, , , extern inline, assert(OOL_EXISTS); assert(AM_INLINED); )
#else
/* "The definition is only used for inlining." So we can emit an OOL one. */
CASEn(23, , , extern inline, assert(0); )
EMIT_OOL(f23n)
CASEo(23, , , extern inline, assert(AM_INLINED);  )
EMIT_OOL_NOTCALLED(f23o)
#endif

#ifdef __GNUC_STDC_INLINE__
CASEn(24, extern inline, , extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(24, extern inline, , extern inline, assert(OOL_EXISTS); assert(AM_INLINED); )
#else
/* "The definition is only used for inlining." So we can emit an OOL one. */
CASEn(24, extern inline, , extern inline, assert(0); )
EMIT_OOL(f24n)
CASEo(24, extern inline, , extern inline, assert(AM_INLINED);  )
EMIT_OOL_NOTCALLED(f24o)
#endif

// bump the middle decl to 'extern inline'

#ifdef __GNUC_STDC_INLINE__
CASEn(25, inline, extern inline, extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(25, inline, extern inline, extern inline, assert(OOL_EXISTS); assert(AM_INLINED); )
#else
/* "The definition is only used for inlining." So we can emit an OOL one.
 * Except that if we declare it also as "[regular] inline", that gets undone. */
CASEn(25, inline, extern inline, extern inline, assert(OOL_EXISTS); assert(!AM_INLINED);  )
CASEo(25, inline, extern inline, extern inline, assert(OOL_EXISTS); assert(AM_INLINED);  )
#endif

#ifdef __GNUC_STDC_INLINE__
CASEn(26, , extern inline, extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(26, , extern inline, extern inline, assert(OOL_EXISTS); assert(AM_INLINED); )
#else
/* "The definition is only used for inlining." So we can emit an OOL one. */
CASEn(26, , extern inline, extern inline, assert(0); )
EMIT_OOL(f26n)
CASEo(26, , extern inline, extern inline, assert(AM_INLINED);  )
EMIT_OOL_NOTCALLED(f26o)
#endif

#ifdef __GNUC_STDC_INLINE__
CASEn(27, extern inline, extern inline, extern inline, assert(OOL_EXISTS); assert(!AM_INLINED); )
CASEo(27, extern inline, extern inline, extern inline, assert(OOL_EXISTS); assert(AM_INLINED); )
#else
/* "The definition is only used for inlining." So we can emit an OOL one. */
CASEn(27, extern inline, extern inline, extern inline, assert(0); )
EMIT_OOL(f27n)
CASEo(27, extern inline, extern inline, extern inline, assert(AM_INLINED);  )
EMIT_OOL_NOTCALLED(f27o)
#endif

int main(void) { return 0; }
