
//Tests for CIL's handling of the packed attribute

//has size 6
struct s1 {
  short a;
  char b;
  short c;
} ;
//Duplicate array declarations force CIL (and gcc) to constant-fold and
//ensure sizeof(struct s1) equals 6:
extern int size6[6U];
extern int size6[sizeof(struct s1)];

//has size 5
struct s2 {
  short a;
  char b;
  short c;
}  __attribute__((packed));
extern int size5[5U];
extern int size5[sizeof(struct s2)];

//has size 5
struct s3 {
  short a  __attribute__((packed));
  char b   __attribute__((packed));
  short c  __attribute__((packed));
};
extern int size5[sizeof(struct s3)];

//has size 6.  The first field has alignment 2, 
// so the whole struct has alignment 2.
struct s4 {
  short a;
  char b   __attribute__((packed));
  short c  __attribute__((packed));
};
extern int size6[sizeof(struct s4)];

//has size 5
struct s5 {
  short a ;
  char b   __attribute__((packed));
  short c  __attribute__((packed));
} __attribute__((packed));
extern int size5[sizeof(struct s5)];

//has size 7.  s1 has size 6, and the packed attribute here applies only to s6,
// not transitively to s1.
struct s6 {
  char a ;
  struct s1 s;
} __attribute__((packed));
extern int size7[7U];
extern int size7[sizeof(struct s6)];

#pragma cilnoremove("size5", "size6", "size7")

int main() {
  return 0;
}
