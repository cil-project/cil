typedef char *  va_list;

int (__cdecl *foo)(int (__stdcall * )(void), int);

struct {
  int (__cdecl *foo)(void);
  int x, y, z __attribute__((packed));
  int u : 5, w : 4 __attribute__((packed));
};

int inline const *fooinline(int *x) {
  int __attribute__((foo)) a, b __attribute__((bar));
  return (const int *)x;
}
  
