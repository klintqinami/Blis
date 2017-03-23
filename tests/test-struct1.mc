struct foo {
  int a;
};

int main()
{
  struct foo bar;
  bar.a = 42;
  print(bar.a);

  return 0;
}
