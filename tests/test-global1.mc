int a;
int b;

void print_a()
{
  printi(a);
}

void print_b()
{
  printi(b);
}

void incab()
{
  a = a + 1;
  b = b + 1;
}

int main()
{
  a = 42;
  b = 21;
  print_a();
  print_b();
  incab();
  print_a();
  print_b();
  return 0;
}
