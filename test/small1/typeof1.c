/* This test case was reported by Robert. It seems that CIL ought to 
 * understand that typeof is an abbreviation */

void foo();

__typeof(foo) a;
void a() {}

void b(void);
extern __typeof(a) b __attribute__ ((alias ("a")));


int main(void) 
{
  a();
  b();
  return 0;
}
