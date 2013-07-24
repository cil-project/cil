typedef struct
{
  char *key;
  char *value;
} T1;

typedef struct
{
  long type;
  char *value;
} T3;

T1 a[] =
{
  {
    "",
    ((char *)&((T3) {1, (char *) 1}))
  }
};
int main() {
   T3 *pt3 = (T3*)a[0].value;
   return pt3->value;
}
