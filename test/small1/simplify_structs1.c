
struct two {
  int i_1;
  int i_2;
};

struct three {
  int i_0;
  struct two i_1and2;
};

struct three global1 = { 0, {10, 20}};

//Try an external declaration:
extern struct three externstruct; //not split

extern void bar(struct three arg);

extern struct three bar2(struct three arg);

int main() {
  struct three local1 = externstruct;
  struct three local2 = externstruct;
  struct three local3 = externstruct;
  //bar(global1);

  bar(local1);
  local3 = bar2(local2); //local 2 is not split

  // externstruct = bar(externstruct);

  
  return 0;
}
