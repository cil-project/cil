
typedef struct {
  int *a[20];
  int b;
} STR;

STR glob;


void main(STR *s) {
  STR loc = glob;

  *s = glob;
}
  
