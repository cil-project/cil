
/* Various ways to place attributes */
char * __cdecl asctime(const struct tm *);
char * __stdcall asctime(const struct tm *);
unsigned long __cdecl _exception_code(void);

unsigned long 
__stdcall
Int64ShllMod32 (void (__stdcall *)());

inline unsigned long
__stdcall
Int64ShlrMod32 ( int Value);

typedef struct {
  int (__stdcall *foo)();
} T1;

typedef int (_cdecl *BAR)();

void __stdcall foo(int x) {
  return;
}

void main() {
  struct tm thetime;
  char *t = asctime(& thetime);
  unsigned long l = Int64ShllMod32( & foo );
}
