struct foo {
  int a;
};

foo my_func() {
  foo ret;
  ret.a = 0;
  return ret;
}

int main()
{
  my_func().a = 2; // Error: my_func() isn't an lvalue
}
