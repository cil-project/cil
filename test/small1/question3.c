int main() {
  int x, y, z;
  int keep1   = x ? y : z;
  int unfold1 = x ? y : z++;
  int unfold2 = x ? y++ : z;
  int unfold3 = x ? y++ : z++;
  int keep2   = x++ ? y : z;
  int unfold4 = x++ ? y : z++;
  int unfold5 = x++ ? y++ : z;
  int unfold6 = x++ ? y++ : z++;
  return 0;
}
