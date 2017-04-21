struct foo {
  int a;
};

struct foo my_func()
{
  struct foo ret;
  ret.a = 42;
  return ret;
}

int main()
{
  printi(my_func().a);

  return 0;
}
