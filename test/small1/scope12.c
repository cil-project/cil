#define WORD int

static inline char f()
{
  typedef int unused_word_type;

  unused_word_type ok;
   return 1;
}

static WORD unused_word_type[1000];

int main() {
  WORD* x = unused_word_type;
  return 0;
}
