struct foo {
  int a;
  int b;
};

int main()
{
  struct foo bar;
  bar.a = 42;
  bar.b = 1;
  printi(bar.a + bar.b);

  return 0;
}
