struct foo {
  int a;
};

foo my_func()
{
  foo ret;
  ret.a = 42;
  return ret;
}

int main()
{
  print(my_func().a);

  return 0;
}
