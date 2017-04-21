struct foo {
  int a;
};

int main()
{
  struct foo bar;
  bar.a = 42;
  printi(bar.a);

  return 0;
}
