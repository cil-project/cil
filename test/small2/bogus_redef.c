// provoke bogus "redefinition" message

int foo()
{
  // call before decl
  return bar();
}

// now define statically
static      // comment me out to make problem disappear
int bar()
{
  return 4;
}

int main()
{
  return foo() - 4;
}
