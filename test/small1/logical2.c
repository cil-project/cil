int main() {
  int x = 0;
  int keep1 = x && x;
  int keep2 = x || x;
  int keep3 = x++ && x;
  int keep4 = x++ || x;
  int unfold1 = x && x++;
  int unfold2 = x || ++x;
  return 0;
}
