// structattr.c
// testing some placement of 'attribute' w.r.t. 'struct' etc.

struct A {
  int x;
} __attribute__((packed));

struct B {
  int x;
} __attribute__((packed)) b;

#if 1
// this is allowed by gcc, but I don't want to implement it because
// it means somehow merging all the attributes across all the
// forward decls and the defn ...
//struct __attribute__((packed)) C;

struct __attribute__((packed)) C {
  int x;
};

struct __attribute__((packed)) D {
  int x;
} d;
#endif // 0/1

// not allowed
//  struct E __attribute__((packed)) {
//    int x;
//  };


int main()
{
  return 0;
}
