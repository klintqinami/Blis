int foo = 2;

@gpu int bar()
{
  return foo;
}

int main()
{
  return 0;
}
