void foo(bool i)
{
  int i; /* Should hide the formal i */

  i = 42;
  printi(i + i);
}

int main()
{
  foo(true);
  return 0;
}
