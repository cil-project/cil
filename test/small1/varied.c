/* An example with various kinds of pointers */

typedef struct list {
  struct list *next; // We'll use this safely
  char *data;
} LIST;

#pragma boxpoly("copy")
void *copy(void *x) {
  return x;
}

int ga[8];

int **w;

int main() {
  int x;
  int * px = &x;
  int * * qx = & px; // SEQ to FSEQ to int
  
  int * * c = copy(qx);
  
  if(x) {
    px = & ga[5];
  } else {
    px ++;
  }
  c += *px;

  {
    char * pw = &w;
    char * * cpw = copy(& pw);

    x = * * cpw;
  }
}
