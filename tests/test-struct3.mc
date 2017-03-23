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
  print(my_func().a);

  return 0;
}
