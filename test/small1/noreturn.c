void direct() __attribute__((noreturn));
void (*indirect)() __attribute__((noreturn)) = direct;

void caller()
{
  direct();
  indirect();
}


int main() {
  return 0;
}
