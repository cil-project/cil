short S;

int f(int x, int y)
{
  return x+y;
}

int main (void) {
  return f(1, S++);
}
