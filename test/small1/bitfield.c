#ifdef _GNUCC
#define _int64 long long
#endif

typedef struct {
  char chr;
  char srt;
  int a : 4;
  int b : 5;
  _int64 c : 16;
  int d : 8;
} S1;

int g1,g2,g3,g4;
extern int bar(int, int);

int foo(S1 *s1) {
  bar(1, s1->chr);
  bar(2, s1->srt);
  bar(3, s1->a);
  bar(4, s1->b);
  bar(5, s1->c);
  bar(6, s1->d);
}
